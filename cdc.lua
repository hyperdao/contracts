--Collaterised Debt Commitments
-- cdc 
type State = 'NOT_INITED' | 'COMMON' | 'STOPPED'


type Storage = {
	admin:string,
	state:string,
        collateralAsset: string,
	annualStabilityFee:string,
	liquidationRatio:string,
	liquidationPenalty:string,
	liquidationDiscount:string,
	priceFeederAddr:string,
	stableTokenAddr:string,
	totalCollectedStablityFee:int,
	totalLiquidationPenalty:int,
	proxy:string,
	stopTime:int,
	annualStabilityFeeList:Array<string>	  ---
}

-- events: Stopped

var M = Contract<Storage>()

let function get_from_address()
    -- 支持合约作为代币持有者
    var from_address: string
    let prev_contract_id = get_prev_call_frame_contract_address()
    if prev_contract_id and is_valid_contract_address(prev_contract_id) then
        -- 如果来源方是合约时
        from_address = prev_contract_id
    else
        from_address = caller_address
    end
    return from_address
end

let function checkAdmin(self: table)
    if self.storage.admin ~= get_from_address() then
        return error("you are not admin, can't call this function")
    end
end

let function checkState(M: table)
    if M.storage.state ~= 'COMMON' then
        return error("state error, now state is " .. tostring(M.storage.state))
    end
end


-- parse a,b,c format string to [a,b,c]
let function parse_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed ~= count) then
        return error(error_msg)
    end
    return parsed
end

let function parse_at_least_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed < count) then
        return error(error_msg)
    end
    return parsed
end

-------------------------------------------------------------------------

offline function M:getStabilityFee(arg:string)
	let proxy = self.storage.proxy
	let r = delegate_call(proxy, 'getStabilityFee', arg)
	return r
end

offline function M:isNeedLiquidation(arg:string)
	let proxy = self.storage.proxy
	let r = delegate_call(proxy, 'isNeedLiquidation', arg)
	return r
end

offline function M:getLiquidableInfo(arg:string)
	let proxy = self.storage.proxy
	let r = delegate_call(proxy, 'getLiquidableInfo', arg)
	return r	
end


offline function M:getCdc(cdc_id:string)
	let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return "{}"
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	let proxy = self.storage.proxy
	let stabilityFee = delegate_call(proxy, 'getStabilityFee', cdc_id)
	cdc_info['stabilityFee'] = stabilityFee
	cdc_info['cdcId'] = cdc_id
	
	let needLiquidate = delegate_call(proxy, 'isNeedLiquidation', cdc_id)
	cdc_info['isNeedLiquidation'] = needLiquidate

	let r = json.dumps(cdc_info)
	return r
end

--{"admin":"con3rrr53ghjp3","collateral":"BTC","AnnualStabilityFee":"0.13","liquidationRatio":"1.25","liquidationPenalty":"0.13","liquidationDiscount":"0.03","stableTokenAddr":"conr34g0rt4tkg","priceFeederAddr":"conbnoet0xfv"}
offline function M:getInfo(arg:string)
	let info = {}
	
	info['admin']=self.storage.admin
	info['state']=self.storage.state
	info['collateralAsset']=self.storage.collateralAsset
	info['annualStabilityFee']=self.storage.annualStabilityFee
	info['liquidationRatio']=self.storage.liquidationRatio
	info['liquidationPenalty']=self.storage.liquidationPenalty
	info['liquidationDiscount']=self.storage.liquidationDiscount
	info['stableTokenAddr']=self.storage.stableTokenAddr
	info['priceFeederAddr']=self.storage.priceFeederAddr
	info['totalCollectedStablityFee']=self.storage.totalCollectedStablityFee
	info['totalLiquidationPenalty']=self.storage.totalLiquidationPenalty
	info['proxy']=self.storage.proxy
	
	info['annualStabilityFeeList']=self.storage.annualStabilityFeeList
	let r = json.dumps(info)
	
	return r
end



let function arrayContains(col: Array<object>, item: object)
    if not item then
        return false
    end
    var value: object
    for _, value in ipairs(col) do
        if value == item then
            return true
        end
    end
    return false
end
-----------------------------------------------------------------------------------------------------------------------

function M:init()
    self.storage.admin = get_from_address()
	self.storage.state = 'NOT_INITED'
    self.storage.collateralAsset = ''
	self.storage.annualStabilityFee = ''
	self.storage.liquidationRatio = ''
	self.storage.liquidationPenalty = ''
	self.storage.liquidationDiscount = ''
	self.storage.stableTokenAddr = ''
	self.storage.priceFeederAddr = ''
	self.storage.totalCollectedStablityFee = 0
	self.storage.totalLiquidationPenalty = 0
	self.storage.proxy = ''
	self.storage.stopTime = 0
	self.storage.annualStabilityFeeList = []
    print("cdc contract created")
end


--arg: collateralAsset,AnnualStabilityFee,liquidationRatio,liquidationPenalty,liquidationDiscount,priceFeederAddr,stableTokenAddr,proxyAddr
function M:init_config(arg: string)
	if self.storage.state ~= 'NOT_INITED' then
        return error("this contract inited before")
    end
	let parsed:Array<string> = totable(parse_args(arg, 8, "arg format error, need format: collateralAsset,annualStabilityFee,liquidationRatio,liquidationPenalty,liquidationDiscount,priceFeederAddr,stableTokenAddr,proxyAddr"))
    let info:Map<string> = {collateralAsset: parsed[1],annualStabilityFee:parsed[2],liquidationRatio: parsed[3],liquidationPenalty: parsed[4],liquidationDiscount: parsed[5], priceFeederAddr: parsed[6],stableTokenAddr: parsed[7],proxyAddr:parsed[8]}
    
	let collateralAsset = info.collateralAsset

	let annualStabilityFee = info.annualStabilityFee
	let sn_annualStabilityFee = safemath.safenumber(annualStabilityFee)
	if not sn_annualStabilityFee then
		return error("annualStabilityFee is not number")
	end
	let sn_0 = safemath.safenumber(0)
	let sn_1 = safemath.safenumber(1)
	if safemath.number_lte(sn_annualStabilityFee,sn_0) then
		return error("annualStabilityFee must > 0%")
	end
	
	let liquidationRatio = info.liquidationRatio
	let sn_liquidationRatio = safemath.safenumber(liquidationRatio)
	if not sn_liquidationRatio then
		return error("liquidationRatio is not number")
	end
	if safemath.number_lt(sn_liquidationRatio,sn_1) then
		return error("liquidationRatio must > 100%")
	end
	
	let liquidationPenalty = info.liquidationPenalty
	if safemath.number_lte(safemath.safenumber(liquidationPenalty),sn_0) then
		return error("liquidationPenalty must > 0%")
	end
	
	let liquidationDiscount = info.liquidationDiscount
	let sn_liquidationDiscount = safemath.safenumber(liquidationDiscount)
	if not sn_liquidationDiscount then
		return error("liquidationDiscount is not number")
	end
	if safemath.number_lte(sn_liquidationDiscount,sn_0) then
		return error("liquidationPenalty must > 0%")
	end	
	if safemath.number_gte(sn_liquidationDiscount,sn_1) then
		return error("liquidationPenalty must < 100%")
	end

	let priceFeederAddr = info.priceFeederAddr
	let stableTokenAddr = info.stableTokenAddr
	let proxyAddr = info.proxyAddr
	
	if not is_valid_contract_address(priceFeederAddr) then
		return error("priceFeederAddr not contract")
	end
	let priceFeederContract:object = import_contract_from_address(priceFeederAddr)
	let baseAsset = priceFeederContract:baseAsset('')
	if baseAsset~=collateralAsset then
		return error("priceFeederContract's baseAsset is not cdc's collateralAsset")
	end
	let quotaAsset = priceFeederContract:quotaAsset('')
	if quotaAsset~=stableTokenAddr then
		return error("priceFeederContract's quotaAsset is not cdc's stableTokenAddr")
	end
	
	if not is_valid_contract_address(stableTokenAddr) then
		return error("stableTokenAddr not contract")
	end
	let stableTokenContract:object = import_contract_from_address(stableTokenAddr)
	let minter = stableTokenContract:minter('')
	let cur_con_addr = get_current_contract_address()
	if not arrayContains(minter,cur_con_addr) then
		return error("stableTokenContract's minter is not cdc contract")
	end
	
	if not is_valid_contract_address(proxyAddr) then
		return error("proxyAddr not contract")
	end
	
	self.storage.collateralAsset = collateralAsset
	self.storage.annualStabilityFee = annualStabilityFee
	self.storage.liquidationRatio = liquidationRatio
	self.storage.liquidationPenalty = liquidationPenalty
	self.storage.liquidationDiscount = liquidationDiscount
	self.storage.priceFeederAddr = priceFeederAddr
	self.storage.stableTokenAddr = stableTokenAddr
	self.storage.totalCollectedStablityFee = 0
	self.storage.totalLiquidationPenalty = 0
	self.storage.proxy = proxyAddr

	let sec_since_epoch = get_chain_now()
	if (sec_since_epoch==nil) or (sec_since_epoch==0) then
		return error("error get_chain_now :"..tostring(sec_since_epoch))
	end
		
	let stabilityFeeRecord = tostring(sec_since_epoch)..","..annualStabilityFee
	self.storage.annualStabilityFeeList = [stabilityFeeRecord]
	self.storage.state = 'COMMON'
	
	emit Inited(json.dumps(info))
	return "OK"
end


function M:on_deposit_asset(jsonArgs: string)
	checkState(self)
	let proxy = self.storage.proxy
	let from_address = get_from_address()
	let arg_info = totable(json.loads(jsonArgs))
	arg_info['from_address'] = from_address
	let r = delegate_call(proxy, 'on_deposit_asset_imp', json.dumps(arg_info))
	return r
end

function M:closeCdc(arg:string)
	checkState(self)
	let proxy = self.storage.proxy
	let from_address = get_from_address()
	let r = delegate_call(proxy, 'closeCdc', from_address..","..arg)
	return r
end

function M:liquidate(arg:string)
	checkState(self)
	let proxy = self.storage.proxy
	let from_address = get_from_address()
	let r = delegate_call(proxy, 'liquidate',from_address..","..arg)
	return r
end

function M:changeProxy(newProxy:string)
	checkAdmin(self)
	if not is_valid_contract_address(newProxy) then
		return error("newProxy is not contract")
	end
	if newProxy == self.storage.proxy then
		return error("new proxy is same as old")
	end
	
	---let stableTokenContract:object = import_contract_from_address(self.storage.stableTokenAddr)
	---stableTokenContract:changeMinter(newProxy)
		
	self.storage.proxy = newProxy
	emit ChangeProxy(newProxy)
	return "OK"
end

function M:changeAdmin(newAdmin:string)
	checkAdmin(self)
	if not is_valid_address(newAdmin) then
		return error("newAdmin is not valid address")
	end
	if newAdmin == self.storage.admin then
		return error("new admin is same as old")
	end
	self.storage.admin = newAdmin
	emit ChangeAdmin(newAdmin)
	return "OK"
end


function M:setAnnualStabilityFee(newAnnualStabilityFee:string)
	checkAdmin(self)
	let annualStabilityFeeList = self.storage.annualStabilityFeeList 
	let count = #annualStabilityFeeList
	let now = tointeger(get_chain_now() or 0)
	let sn_newAnnualStabilityFee = safemath.safenumber(newAnnualStabilityFee)
	if not sn_newAnnualStabilityFee then
		return error("newAnnualStabilityFee is not number")
	end
	let sn_0 = safemath.safenumber(0)
	if safemath.number_lte(sn_newAnnualStabilityFee,sn_0) then
		return error("newAnnualStabilityFee must > 0")
	end
	
	if count > 0 then  --- check last
		let parsed:Array<string> = totable(parse_args(annualStabilityFeeList[count],2,"annualStabilityFeeList record wrong"))
		let secPoint = tointeger(parsed[1] or 0)
		if (secPoint >= now) then
			return error("time error ,last secPoint:"..tostring(secPoint).." now:"..tostring(now))
		end
		let last_annualStabilityFee = safemath.safenumber(parsed[2])
		if safemath.number_eq(sn_newAnnualStabilityFee,last_annualStabilityFee) then
			return error("same as last one")
		end
	end
	table.append(annualStabilityFeeList,tostring(now)..","..newAnnualStabilityFee)
	self.storage.annualStabilityFeeList = annualStabilityFeeList
	self.storage.annualStabilityFee = newAnnualStabilityFee
	emit SetAnnualStabilityFee(newAnnualStabilityFee)
	return "OK"
end

offline function M:getAnnualStabilityFeeList(_:string)
    let r = self.storage.annualStabilityFeeList
	return r
end

offline function M:getAnnualStabilityFee(_:string)
    let r = self.storage.annualStabilityFee
	return r
end


function M:setLiquidationRatio(newLiquidationRatio:string)
	checkAdmin(self)
	let sn_newLiquidationRatio = safemath.safenumber(newLiquidationRatio)
	let sn_1 = safemath.safenumber(1)
	if safemath.number_lt(sn_newLiquidationRatio,sn_1) then
		return error("newLiquidationRatio must > 100%")
	end
	
	if(newLiquidationRatio == self.storage.liquidationRatio) then
		return error("newLiquidationRatio is same as old")
	end
	
	self.storage.liquidationRatio = newLiquidationRatio	
	emit SetLiquidationRatio(newLiquidationRatio)
	return "OK"
end

offline M:getLiquidationRatio(_:string)
    let r - self.storage.liquidationRatio
	return r
end

function M:setLiquidationPenalty(newLiquidationPenalty:string)
	checkAdmin(self)
	let sn_newLiquidationPenalty = safemath.safenumber(newLiquidationPenalty)
	if not sn_newLiquidationPenalty then
		return error("newLiquidationPenalty is not number")
	end
	let sn_0 = safemath.safenumber(0)
	if safemath.number_lt(sn_newLiquidationPenalty,sn_0) then
		return error("newCollateralizationRatio must >= 0%")
	end

	if newLiquidationPenalty==self.storage.liquidationPenalty then
		return error("new liquidationPenalty is same as old")
	end
	self.storage.liquidationPenalty = newLiquidationPenalty	
	emit SetLiquidationPenalty(newLiquidationPenalty)
	return "OK"
end

offline function M:getLiquidationPenalty(_:string)
    let r = self.storage.liquidationPenalty
	return r
end

function M:setLiquidationDiscount(newLiquidationDiscount:string)
	checkAdmin(self)
	let sn_newLiquidationDiscount = safemath.safenumber(newLiquidationDiscount)
	if not sn_newLiquidationDiscount then
		return error("newLiquidationDiscount is not number")
	end
	let sn_0 = safemath.safenumber(0)
	let sn_1 = safemath.safenumber(1)
	if safemath.number_lt(sn_newLiquidationDiscount,sn_0) then
		return error("newCollateralizationRatio must >= 0%")
	end
	if safemath.number_gte(sn_newLiquidationDiscount,sn_1) then
		return error("newCollateralizationRatio must < 100%")
	end
	
	if newLiquidationDiscount==self.storage.liquidationDiscount then
		return error("new liquidationDiscount is same as old")
	end
	self.storage.liquidationDiscount = newLiquidationDiscount	
	emit SetLiquidationDiscount(newLiquidationDiscount)
	return "OK"
end

offline function M:getLiquidationDiscount(_:string)
    let r = self.storage.liquidationDiscount
	return r
end


function M:setPriceFeederAddr(priceFeederAddr:string)
	checkAdmin(self)
	if not is_valid_contract_address(priceFeederAddr) then
		return error("priceFeederAddr not contract")
	end
	if priceFeederAddr==self.storage.priceFeederAddr then
		return error("new priceFeederAddr is same as old")
	end
	let priceFeederContract:object = import_contract_from_address(priceFeederAddr)
	let baseAsset = priceFeederContract:baseAsset('')
	if baseAsset~=self.storage.collateralAsset then
		return error("priceFeederContract's baseAsset is not cdc's collateralAsset")
	end
	let quotaAsset = priceFeederContract:quotaAsset('')
	if quotaAsset~=self.storage.stableTokenAddr then
		return error("priceFeederContract's quotaAsset is not cdc's stableTokenAddr")
	end
	self.storage.priceFeederAddr = priceFeederAddr
	return "OK"
end
	

function M:globalLiquidate(arg:string)
	checkAdmin(self)
	checkState(self)
	self.storage.state = 'STOPPED'
	self.storage.stopTime = tointeger(get_chain_now())
	emit GlobalLiquidate("")
	return "OK"
end


function M:closeContract(arg:string)
	checkAdmin(self)
	if self.storage.state~='STOPPED' then
		return error("can't call this api before globalLiquidate")
	end
	let collateralAsset = self.storage.collateralAsset
	let balance = tointeger(get_contract_balance_amount(get_current_contract_address(),collateralAsset))
	if balance>0 then
		let res = transfer_from_contract_to_address(self.storage.admin, collateralAsset, balance)
		if res ~= 0 then
			return error("transfer from contract to " .. (self.storage.admin) .. " realAmount " .. tostring(balance) .. " error, error code is " .. tostring(res))
		end
	end
	
	emit CloseContract(tostring(balance))
	return "OK"
end

function M:takeBackCollateralByCdc(arg:string)
	let proxy = self.storage.proxy
	let r = delegate_call(proxy, 'takeBackCollateralByCdc', arg)
	return r
end

function M:takeBackCollateralByToken(arg:string)
	let proxy = self.storage.proxy
	let r = delegate_call(proxy, 'takeBackCollateralByToken', arg)
	return r
end

--arg:cdcId,to_address
function M:transferCdc(arg:string)
	let parsed = parse_args(arg,2,"arg format wrong,need format:cdc_id,to_address")
	let cdc_id = tostring(parsed[1])
	let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdc_id)
	end
	let to_address = tostring(parsed[2])
	if not is_valid_address(to_address) then
		return error("to_address is not valid")
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	
	let from_address = get_from_address()
	if from_address~=cdc_info['owner'] then
		return error("you are not cdc owner")
	end
	
	cdc_info['owner'] = to_address
	fast_map_set("cdc",cdc_id,json.dumps(cdc_info))

	emit TransferCdc(json.dumps({from_address:from_address,to_address:to_address,cdcId:cdc_id}))
	return "OK"
end

---args:cdc_id,expandLoanAmount
function M:expandLoan(arg:string)
	checkState(self)
	let proxy = self.storage.proxy
	let from_address = get_from_address()
	let r = delegate_call(proxy, 'expandLoan',from_address..","..arg)
	return r
end

---args:cdc_id,widrawCollateralAmount
function M:widrawCollateral(arg:string)
	checkState(self)
	let proxy = self.storage.proxy
	let from_address = get_from_address()
	let r = delegate_call(proxy, 'widrawCollateral',from_address..","..arg)
	return r
end

---args:cdc_id,payBackAmount(include fee)
function M:payBack(arg:string)
	checkState(self)
	let proxy = self.storage.proxy
	let from_address = get_from_address()
	let r = delegate_call(proxy, 'payBack',from_address..","..arg)
	return r
end

	
function M:on_destroy()
    error("can't destroy cdc contract")
end

return M



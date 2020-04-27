--proxy contracts

type State = 'NOT_INITED' | 'COMMON' | 'STOPPED'

type Storage = {
	admin:string,
	state:string,
	symbol:string,
    cdcContractAddr: string,	
	feederContractAddr: string, ---
	stableTokenAddr: string,
	tokenDebtInfo: Map<string>
}


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

let function can_distribute(self:table,wishedAmount:string,addedAmount:int)
    let sn_wishedAmount = safemath.safenumber(amount)
	if not sn_wishedAmount then
	   return error("whishAmoun is not a number")
	end
	let sn_0 = safemath.safenumber(0)
	if safemath.number_lte(sn_wishedAmount,sn_0) then
		return error("wishedAmount must > 0")
	end
	
	
    let cdcContractAddr:object = import_contract_from_address(self.storage.cdcContractAddr) ---get the cdc object
	let liquidationRatio = cdcContractAddr:liquidationRatio('')
	let sn_liquidationRatio = safemath.safenumber(liquidationRatio)
	
	let feederContractAddr:object = import_contract_from_address(self.storage.feederContractAddr)
	let price = feederContractAddr:getPrice('')
	let sn_price = safemath.safenumber(price)
    
    let sn_totalvalue_wished = safemath.safe_number_multiply(sn_price,sn_wishedAmount)
	let sn_addedAmount = safemath.safenumber(addedAmount)
	let sn_totalvalue_collectal = safemath.safe_number_multiply(sn_price,sn_addedAmount)
	
	let sn_ratio = safemath.safe_number_div(sn_totalvalue_collectal,sn_totalvalue_wished)
	if safemath.safe_number_lt(sn_ratio,sn_liquidationRatio) then
	    return error("u need amount exceed reality")
	end    	
    return true
end


let function mint(self:table,address:string,amount: string)
    let stableTokenAddrContract:object = import_contract_from_address(self.storage.stableTokenAddrContract)
	stableTokenAddrContract:mint(address..','..amount)
end


function M:init()
    self.storage.admin = ''
    self.storage.state= 'NOT_INITED'
    self.storage.cdcContractAddr = ''
	self.stor.feederContractAddr =''
    self.storage.stableTokenAddr = ''    
	self.storage.tokenDebtInfo = {}
	self.storage.symbol = ''
end

function M:init_config(arg:string)
    if self.storage.state ~= 'NOT_INITED' then
        return error("this contract inited before")
    end
    let parsed:Array<string> = totable(parse_args(arg, 5, "arg format error, need format: admin,symbol,cdcContractAddr,feederContractAddr,stableTokenAddr"))
	self.storage.admin = tostring(parse_args[1])
	self.storage.symbol = tostring(param_args[2])
	self.feederContractAddr = tostring(param_args[3])
	self.storage.cdcContractAddr = tostring(parse_args[4])
	self.storage.stableTokenAddr = tostring(parse_args[5])
	
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

function M:on_deposit_asset(jsonArgs: string)
    return error("not supported deposit to  proxy contract")
end



function M:on_deposit_asset_imp(jsonstrArgs:string)
    if self.storage.state ~= 'NOT_INITED' then
        return error("this contract inited before")
    end
	
	let arg = json.loads(jsonstrArgs)
    let addedAmount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
	if symbol ~= self.storage.symbol then
	    return error("deposit error tokens into this contract.")
	end
	let from_address = tostring(arg.from_address)
    let param = tostring(arg.param)
	let parsed:Array<string> = totable(parse_args(param,2,"arg format error, need format:openCdc,amount or addCollateral,cdc_id"))
    if parsed[1] == 'openCdc' then
	---warning to implement
	    let wishedAmount = parsed[2]
	    can_distribute(self,wishedAmount,addedAmount)
	    cdc_info = {}
		cdc_info['owner'] = from_address
		cdc_info['collateralAmount'] = addedAmount
		cdc_info['stableTokenAmount'] = tointeger(wishedAmount)
		cdc_info['secSinceEpoch'] = tointeger(get_chain_now() or 0)
		cdc_id = tostring('12213434')
		fast_map_set("cdc",cdc_id,json.dumps(cdc_info))
		cdc_info['cdcId'] = cdc_id
		mint(self,from_address,wishedAmount)
		emit OpenCdc(json.dumps(cdc_info))
	    return  cdc_id
    end
	if parsed[1] == 'addCollateral' then
	    cdc_id = tostring(parsed[2])
		let cdc_info_obj = fast_map_get("cdc",cdc_id)
		if cdc_info_obj == nil then
		    return error("cdc not exist , cdc_id:"..cdc_id)
		end
		let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
		if from_address ~= cdc_info['owner'] then
		    return error("only owner can do this operition")
		end
	    cdc_info['collateralAmount'] = tointeger(cdc_info['collateralAmount']) + addedAmount
		fast_map_set("cdc",cdc_id,json.dumps(cdc_info))
		emit AddCollateral(json.dumps({cdc_id:cdc_id,addAmount:addedAmount}))
	    return "OK"---need emit
	end
end

function M:expandLoan(args:string)

end

function M:getStabilityFee(cdc_id:string)
    let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdc_id)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	let stableTokenAmount = cdc_info['stableTokenAmount']
	let secPoint = cdc_info['secSinceEpoch']
	let sn_stableTokenAmount = safemath.safenumber(stableTokenAmount)
	let sn_secPoint = safemath.safenumber(sn_stableTokenAmount)
	let time_now = tointeger(get_chain_now() or 0)
	let sn_time_now = safemath.safenumber(time_now)
	
	let sn_daysec = safemath.safenumber(86400)
	let sn_year  = safemath.safenumber(365)
	
	let sn_length = safemath.safe_number_minus(sn_time_now,sn_secPoint)
	let sn_day = safemath.safe_number_idiv(sn_length,sn_daysec)
	
	
	let cdcContractAddr:object = import_contract_from_address(self.storage.cdcContractAddr)
	let annualStabilityFee = cdcContractAddr:annualStabilityFee('')
	let sn_annualStabilityFee = safemath.safenumber(annualStabilityFee)
	let sn_stablityFee_t = safemath.safe_number_multiply(sn_day,sn_annualStabilityFee)
	let sn_stablityFee = safemath.safe_number_idiv(sn_stablityFee_t,sn_year)
	return sn_stablityFee
end


return M

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
	    return false
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
	    if not can_distribute(self,wishedAmount,addedAmount) then
		    return error("can not distribute")
		end
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
    checkState(self)
	let parsed:Array<string> = totable(parse_args(param,3,"arg format error, need format:from_address,cdc_id or expandLoanAmount"))
	let from_address = tostring(parsed[1])
	let cdc_id = tostring(parsed[2])
	let expandLoanAmount = parsed[3]
	let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdc_id)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	if from_address ~= cdc_info['owner'] then
	    return error("address should be owner.")
	end
	let sn_expandLoanAmount = sfamath.safenumber(expandLoanAmount)
	if not sn_expandLoanAmount then
	    return error("invalid expandloan amount.")
	end
	let sn_0 = safemath.safenumber(0)
	if safemath.number_lte(sn_expandLoanAmount,sn_0) then
		return error("expandLoanAmount must > 0")
	end 
	
	let collateralAmount =  tointeger(cdc_info['collateralAmount'])
	let stableTokenAmount = cdc_info['stableTokenAmount']
	
	let sn_stableTokenAmount = safemath.safenumber(stableTokenAmount)
	sn_stableTokenAmount = safemath.safe_number_add(sn_stableTokenAmount,sn)
	let wishedAmount = safemath.safe_number_to_string(sn_stableTokenAmount)
	if not can_distribute(self,wishedAmount,collateralAmount) then
	    return error("can not distribute.")
	end
	
	let stablityFee = M:getStabilityFee(cdc_id)
	let sn_stabilityFee = safemath.safenumber(stablityFee)
	let sn_realGotAmount = safemath.safe_number_minus(sn_expandLoanAmount,sn_stabilityFee)
	
	if safema.number_lte(sn_realGotAmount,sn_0) then
	    return error("cannot expand loan now, pls add collateral")
	end
	let realGotAmount = safemath.safe_number_to_string(sn_realGotAmount)
	mint(self,from_address,realGotAmount)
	let sn_currentStableAmount = sfamath.safe_number_minus(sn_stableTokenAmount,sn_stabilityFee)
	let currentStableAmount = safemath.safe_number_to_int64(sn_currentStableAmount)
	cdc_info['stableTokenAmount'] = currentStableAmount
	cdc_info['secSinceEpoch'] = tointeger(get_chain_now() or 0)
	fast_map_set("cdc",cdc_id,json.dumps(cdc_info))
	emit ExpandLoan(json.dumps({cdcId:cdc_id,repayFee:stablityFee,from_address:from_address,realGotAmount:realGotAmount,expandLoanAmount:expandLoanAmount}))
	
	return 'OK'
end

offline function M:getStabilityFee(cdc_id:string)
    checkState(self)
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
	return safemath.safe_number_to_int64(sn_stablityFee)
end


offline function M:isNeedLiquidation(cdc_id:string)
    checkState(self)
	let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdc_id)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	
	let collateralAmount =  tointeger(cdc_info['collateralAmount'])
	let stableTokenAmount = tostring(cdc_info['stableTokenAmount'])
	if not can_distribute(self,stableTokenAmount,collateralAmount) then
	  return true
	end
	---let stabilityFee = M:getStabilityFee(cdc_id) consider if we need 
end

offline function M:getLiquidableInfo(cdc_id:string)
    let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdc_id)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	let stabilityFee = M:getStabilityFee(cdc_id)
	let stableTokenAmount = cdc_info['stableTokenAmount']
	let owner = cdc_info['owner']
	let secSinceEpoch = cdc_info['secSinceEpoch']
	let collateralAmount = cdc_info['collateralAmount']
	if not M:isNeedLiquidation(cdc_id) then
	    let sn_stabilityFee = safemath.safenumber(stabilityFee)
		let sn_stableTokenAmount = safemath.safenumber(stableTokenAmount)
		let sn_repayStableTokenAmount = safemath.safe_number_add(sn_stabilityFee,sn_stableTokenAmount)
		let repayStableTokenAmount = safemath.safe_number_to_int64(sn_repayStableTokenAmount)
		
		let result:table = {cdcId:cdc_id,owner:owner,stablityFee:stabilityFee,secSinceEpoch:secSinceEpoch,
		                         collateralAmount:collateralAmount, isNeedLiquidation:false,stableTokenAmount:stableTokenAmount,
						                 repayStableTokenAmount:repayStableTokenAmount}
										 
	    return json.dumps(result)
	end
	
	
	---need to be liquidated.... TODO
end


function M:payBack(args:string)
    heckState(self)
	let parsed:Array<string> = totable(parse_args(param,3,"arg format error, need format:from_address,cdc_id or payBackAmount"))
	let from_address = tostring(parsed[1])
	let cdc_id = tostring(parsed[2])
	let payBackAmount = parsed[3]
	
	let sn_0 = safema.safenumber(0)
	let sn_payBackAmount = safemath.safenumber(payBackAmount)
	if from_address ~= cdc_info['owner'] then
	    return error("address is not correct")
	end
	if not sn_payBackAmount then
	    return error("invalid payBackAmount")
	end
	
	if safemath.number_lte(sn_0) then
	    return error("payBackAmount should be larger than 0")
	end
	
	let stabilityFee = M:getStabilityFee(cdc_id)
	let sn_stabilityFee = safemath.safenumber(stabilityFee)
	
	if safemath.number_lt(sn_payBackAmount,sn_stabilityFee) then
	    return error("payBackAmount should be larger than stablityFee.")
	end
	
	let sn_left_pay_stablitFee = sfamath.safe_number_minus(sn_payBackAmount,sn_stabilityFee)
	let left_pay_stabilitFee = safemath.safe_number_to_int64(sn_left_pay_stablitFee)
	let stableTokenAmount = cdc_id['stableTokenAmount']
	let sn_stableTokenAmount = safemath.safenumber(stableTokenAmount)
	if safemath.number_gt(sn_payBackAmount,sn_stableTokenAmount) then
	    return error("payBackAmount should be less than stableTokenAmount.")
	end
	sn_stableTokenAmount = safemath.safe_number_minus(sn_stableTokenAmount,sn_left_pay_stablitFee)
	
	cdc_id['stableTokenAmount'] = safemath.safe_number_to_int64(sn_stableTokenAmount)
	cdc_id['secSinceEpoch'] = tointeger(get_chain_now() or 0)
    emit PayBack(json.dumps({cdcId:cdc_id,fee:stabilityFee,from_address:from_address,payBackAmount:payBackAmount,repayPrincipal:left_pay_stabilitFee]}))
	let cur_con_addr = get_current_contract_address()
	
	let stableTokenAddrContract:object = import_contract_from_address(self.storage.stableTokenAddrContract)
	stableTokenAddrContract:destoryAndTrans(from_address..','..payBackAmount','..','..'0')
	return "OK"
end

return M

--proxy contracts

type State = 'NOT_INITED' | 'COMMON' | 'STOPPED'

type Storage = {
	admin:string,
	state:string,
	symbol:string,,
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

let function can_distribute(self:table,amount:string)
    let sn_wishedAmount = safemath.safenumber(amount)
	if not sn_wishedAmount then
	   return error("whishAmoun is not a number")
	end
	let sn_0 = safemath.safenumber(0)
	if safemath.number_lte(sn_wishedAmount,sn_0) then
		return error("whishedAmount must > 0")
	end
	
	
    let priceFeederContract:object = import_contract_from_address(self.storage.cdcContractAddr)
	let liquidationRatio = liquidationRatio:liquidationRatio('')
	let sn_liquidationRatio = safemath.safenumber(liquidationRatio)
	
    return false
end


let function mint(self:table,amount: int)

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
	    cdc_info = {}
		cdc_info['owner'] = from_address
		cdc_info['collateralAmount'] = addedAmount
		cdc_info['stableTokenAmount'] = tointeger(parsed[2])
		cdc_info['secSinceEpoch'] = tointeger(get_chain_now() or 0)
		cdc_id = tostring('12213434')
		fast_map_set("cdc",cdc_id,json.dumps(cdc_info))
		cdc_info['cdcId'] = cdc_id
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
		emit AddCollateral(json.dumps({cdc_id:cdc_id,addAmount:addAmount}))
	    return "OK"---need emit
	end
end





return M



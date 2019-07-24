--Collaterised Debt Commitments
-- cdc 
type State = 'NOT_INITED' | 'COMMON' | 'STOPPED'


type Storage = {
	admin:string,
	state:string,
    collateralAsset: string,
	collateralizationRatio:string,
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

offline function M:getStabilityFee(cdcId:string)
	let cdc_info_obj = fast_map_get("cdc",cdcId)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdcId)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	
	var secSinceEpoch = tointeger(cdc_info['secSinceEpoch'] or -1)
	if secSinceEpoch==-1 then
		return error("cdc secSinceEpoch error, cdc_id:"..cdcId)
	end
	let stableTokenAmount = tointeger(cdc_info['stableTokenAmount'] or -1)
	if (stableTokenAmount==-1) then
		return error("cdc stableTokenAmount error, cdc_id:"..cdcId)
	end
	var endTime = tointeger(get_chain_now() or 0)
	let stopTime = self.storage.stopTime
	if stopTime>0 then
		endTime = stopTime
	end
	
	
	let annualStabilityFeeList = self.storage.annualStabilityFeeList
	let count = #annualStabilityFeeList
	var totalSecs:int = 0
	var secs:int = 0
	
	var sn_fee:object
	var sn_totalFee:object = safemath.safenumber(0)
	var sn_annualStabilityFee:object 

	var lastSecPoint:int = 0

	if count > 1 then
		var j:int=count
		var secPoint:int = 0
		while j>=1 do
			let parsed:Array<string> = totable(parse_args(annualStabilityFeeList[j],2,"annualStabilityFeeList record wrong"))
			let secPoint1 = tointeger(parsed[1] or 0)
			sn_annualStabilityFee = safemath.safenumber(parsed[2])
			if j==count then
				lastSecPoint = endTime
			end
			if secSinceEpoch<secPoint1 then
				secs = lastSecPoint - secPoint1
				sn_fee = safemath.number_multiply(safemath.number_div(safemath.number_multiply(sn_annualStabilityFee,stableTokenAmount),31536000),secs)
				sn_totalFee = safemath.number_add(sn_totalFee,sn_fee)
				lastSecPoint = secPoint1
			else
				secs = lastSecPoint - secSinceEpoch
				sn_fee = safemath.number_multiply(safemath.number_div(safemath.number_multiply(sn_annualStabilityFee,stableTokenAmount),31536000),secs)
				sn_totalFee = safemath.number_add(sn_totalFee,sn_fee)
				break
			end
			j = j-1
		end	
	else
		sn_annualStabilityFee = safemath.safenumber(self.storage.annualStabilityFee )
		if endTime <= secSinceEpoch then
			return error("secSinceEpoch in cdc record wrong")
		end
		secs = endTime - secSinceEpoch
		sn_totalFee = safemath.number_multiply(safemath.number_div(safemath.number_multiply(sn_annualStabilityFee,stableTokenAmount),31536000),secs)
	end
	
	var fee = safemath.number_toint(sn_totalFee)
	if fee < 0 then 
		return error("inter error caculate")
	end
	if fee == 0 then
		fee = 1
	elseif(safemath.number_gt(sn_totalFee,safemath.safenumber(fee))) then
		fee = fee + 1
	end
	let result = tostring(fee)
	return result
end

offline function M:isNeedLiquidation(cdcId:string)
	let cdc_info_obj = fast_map_get("cdc",cdcId)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdcId)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	var secSinceEpoch = tointeger(cdc_info['secSinceEpoch'] or -1)
	if secSinceEpoch==-1 then
		return error("cdc secSinceEpoch error, cdc_id:"..cdcId)
	end
	let stableTokenAmount = tointeger(cdc_info['stableTokenAmount'] or -1)
	if (stableTokenAmount==-1) then
		return error("cdc stableTokenAmount error, cdc_id:"..cdcId)
	end
	let collateralAmount = tointeger(cdc_info['collateralAmount'] or -1)
	if (collateralAmount==-1) then
		return error("cdc collateralAmount error, cdc_id:"..cdcId)
	end
	let stableFee = tointeger(delegate_call(get_current_contract_address(), 'getStabilityFee', cdcId))
	--let stableFee = tointeger(self:getStabilityFee(cdcId))
	let sn_liquidationRatio = safemath.safenumber(self.storage.liquidationRatio)
	
	let priceFeederContract:object = import_contract_from_address(self.storage.priceFeederAddr)
	let price = priceFeederContract:getPrice('')
	let sn_price = safemath.safenumber(price)
	if safemath.number_lt(safemath.number_div(safemath.number_multiply(sn_price,collateralAmount),(stableFee+stableTokenAmount)),sn_liquidationRatio) then
		return "true"
	end
	return "false"

end

offline function M:getLiquidableInfo(cdcId:string)
	let cdc_info_obj = fast_map_get("cdc",cdcId)
	if cdc_info_obj == nil then
		return error("cdc not exist,cdc_id:"..cdcId)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	---{"owner":"eergr","collateralAmount":8000000,"stableTokenAmount":9000000000,"secSinceEpoch":956545355,"stabilityFee":3045}
	if not cdc_info  then
		return error("cdc not exist , cdc_id :"..cdcId)
	end
	var secSinceEpoch = tointeger(cdc_info['secSinceEpoch'] or -1)
	if secSinceEpoch==-1 then
		return error("cdc secSinceEpoch error, cdc_id:"..cdcId)
	end
	let stableTokenAmount = tointeger(cdc_info['stableTokenAmount'] or -1)
	if (stableTokenAmount==-1) then
		return error("cdc stableTokenAmount error, cdc_id:"..cdcId)
	end
	let collateralAmount = tointeger(cdc_info['collateralAmount'] or -1)
	if (collateralAmount==-1) then
		return error("cdc collateralAmount error, cdc_id:"..cdcId)
	end
	cdc_info['cdcId'] = cdcId
	let stableFee = tointeger(delegate_call(get_current_contract_address(), 'getStabilityFee', cdcId))
	--let stableFee = tointeger(self:getStabilityFee(cdcId))
	
	let sn_liquidationRatio = safemath.safenumber(self.storage.liquidationRatio)
	
	let priceFeederContract:object = import_contract_from_address(self.storage.priceFeederAddr)
	--let stableTokenAddr = self.storage.stableTokenAddr
	let price = priceFeederContract:getPrice('')
	var isNeedLiquidation = false
	let sn_price = safemath.safenumber(price)
	if safemath.number_lt(safemath.number_div(safemath.number_multiply(sn_price,collateralAmount),(stableFee+stableTokenAmount)),sn_liquidationRatio) then
		isNeedLiquidation = true
	end
	cdc_info["isNeedLiquidation"] = isNeedLiquidation
	cdc_info["stabilityFee"] = stableFee
	var repayStableTokenAmount = stableTokenAmount + stableFee
	cdc_info["repayStableTokenAmount"] = repayStableTokenAmount
	---{"isNeedLiquidation":true,"auctionCollateralAmount":2997,"repayStableTokenAmount":21800000,"curPrice":"7500.0","auctionPrice":"7275.0","penaltyAmount":1600000,"returnAmount":3,"isBadDebt":false}
	if isNeedLiquidation then
		let penaltyAmount = safemath.number_toint(safemath.number_multiply(safemath.safenumber(self.storage.liquidationPenalty),stableTokenAmount))
		repayStableTokenAmount = stableTokenAmount + penaltyAmount + stableFee
		let sn_auctionPrice = safemath.number_minus(sn_price,safemath.number_multiply(safemath.safenumber(self.storage.liquidationDiscount),sn_price))
		
		let sn_auctionCollateralAmount = safemath.number_div(safemath.safenumber(repayStableTokenAmount),sn_auctionPrice)
		var auctionCollateralAmount = safemath.number_toint(sn_auctionCollateralAmount)
		if auctionCollateralAmount == 0 then
			auctionCollateralAmount = 1
		else
			if safemath.number_ne(sn_auctionCollateralAmount,safemath.safenumber(auctionCollateralAmount)) then
				auctionCollateralAmount = auctionCollateralAmount + 1
			end
		end
		var isBadDebt = false
		if auctionCollateralAmount > collateralAmount then
			isBadDebt = true
		end
		let returnAmount = collateralAmount - auctionCollateralAmount
		cdc_info["isBadDebt"] = isBadDebt
		cdc_info["auctionCollateralAmount"] = auctionCollateralAmount
		cdc_info["repayStableTokenAmount"] = repayStableTokenAmount
		cdc_info["curPrice"] = price
		cdc_info["auctionPrice"] = safemath.number_tostring(sn_auctionPrice)
		cdc_info["penaltyAmount"] = penaltyAmount
		cdc_info["returnAmount"] = returnAmount
	end
	
	let r = json.dumps(cdc_info)
	return r
end

-----------------------------------------------------------------------------------------------------------------------

function M:init()
    self.storage.admin = caller_address
	self.storage.state = 'NOT_INITED'
    self.storage.collateralAsset = ''
	self.storage.collateralizationRatio = ''
	self.storage.annualStabilityFee = ''
	self.storage.liquidationRatio = ''
	self.storage.liquidationPenalty = ''
	self.storage.liquidationDiscount = ''
	self.storage.stableTokenAddr = ''
	self.storage.priceFeederAddr = ''
	self.storage.totalCollectedStablityFee = 0
	self.storage.totalLiquidationPenalty = 0
	self.storage.proxy = ''
	self.storage.stopTime=0
	self.storage.annualStabilityFeeList = []
    print("cdc contract created")
end

function M:on_deposit_asset(arg:string)
	return error("can't deposit")
end

function M:on_deposit_asset_imp(jsonstrArgs: string)
    let arg = json.loads(jsonstrArgs)
    let amount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
	let param = tostring(arg.param)

	if (not amount) or (amount <= 0) then
		 return error("deposit should greater than 0")
	end

	if (symbol~=self.storage.collateralAsset) then
		 return error("noly accept deposit asset:"..(self.storage.collateralAsset).." can't deposit asset:"..symbol)
	end
	let from_address = tostring(arg.from_address)
	--print("from address in on_deposit_asset_imp:"..from_address)
	let sec_since_epoch = get_chain_now()
	var cdc_info = {}
	var cdc_info_str = ''
	if param == "openCdc" then
		let priceFeederContract:object = import_contract_from_address(self.storage.priceFeederAddr)
		--let stableTokenAddr = self.storage.stableTokenAddr
		let price = priceFeederContract:getPrice('')
		let sn_price = safemath.safenumber(price)
		
		let sn_collateralizationRatio = safemath.safenumber(self.storage.collateralizationRatio)
		let sn_amount = safemath.safenumber(amount)
		let mintStableTokenAmount = safemath.number_toint(safemath.number_div(safemath.number_multiply(sn_amount,sn_price),sn_collateralizationRatio))
		if mintStableTokenAmount<=0 then
			return error("mintStableTokenAmount <= 0")
		end
		let stableTokenContract:object = import_contract_from_address(self.storage.stableTokenAddr)
		stableTokenContract:mint(from_address..","..tostring(mintStableTokenAmount))
		let txid = get_transaction_id() ---txid 作为cdc id
		---cdc : owner,抵押资产数量,欠稳定币数量,抵押时间
		cdc_info  = {owner:from_address,collateralAmount:amount,stableTokenAmount:mintStableTokenAmount,secSinceEpoch:sec_since_epoch}
		cdc_info_str = json.dumps(cdc_info)
		fast_map_set("cdc",txid,cdc_info_str)
		cdc_info['cdcId'] = txid
		emit OpenCdc(json.dumps(cdc_info))
		
	else  ----addCollateral
		let parsed:Array<string> = totable(parse_args(param, 2, "arg format error, need format: addCollateral,cdc_id or openCdc"))
		if(parsed[1]~="addCollateral") then
			return error("arg format error, need format: addCollateral,cdc_id or openCdc")
		end
		let cdc_id = parsed[2]
		let cdc_info_obj = fast_map_get("cdc",cdc_id)
		if cdc_info_obj == nil then
			return error("cdc not exist , cdc_id:"..cdc_id)
		end
		cdc_info = totable(json.loads(tostring(cdc_info_obj)))

		--check owner ??
		let orig_collateralAmount = tointeger(cdc_info['collateralAmount'] or 0)
		let newCollateralAmount = orig_collateralAmount + amount
		if(newCollateralAmount-amount~=orig_collateralAmount) then
			return error("integer overflow ")
		end

		cdc_info['collateralAmount'] = newCollateralAmount
		cdc_info_str = json.dumps(cdc_info)
		fast_map_set("cdc",cdc_id,cdc_info_str)
		let info = {cdcId:cdc_id,addAmount:amount}
		emit AddCollateral(json.dumps(info))
		--cdc_info
	end	
	return "OK"
end

function M:closeCdc(arg:string)
	let parsed = parse_args(arg,2,"arg format wrong")
	let cdc_id = tostring(parsed[2])
	let cdc_info_obj = fast_map_get("cdc",cdc_id)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdc_id)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	let from_address = tostring(parsed[1])
	--let from_address = get_from_address()
	if from_address~=cdc_info['owner'] then
		return error("you are not cdc owner")
	end
	
	let cur_contract_address = get_current_contract_address()
	--if (delegate_call(cur_contract_address, 'isNeedLiquidation', cdc_id) == "true") then
	--	return error("needLiquidation,can't close")
	--end
	let fee = tointeger(delegate_call(cur_contract_address, 'getStabilityFee', cdc_id))
	
	let stableTokenAmount = tointeger(cdc_info['stableTokenAmount'])
	
	self.storage.totalCollectedStablityFee = self.storage.totalCollectedStablityFee + fee
	
	fast_map_set("cdc",cdc_id,nil)

	let collateralAmount = tointeger(cdc_info['collateralAmount'])
	let res = transfer_from_contract_to_address(from_address, self.storage.collateralAsset, collateralAmount)
	if res ~= 0 then
		return error("transfer from contract to " .. from_address .. " realAmount " .. tostring(collateralAmount) .. " error, error code is " .. tostring(res))
	end
	
	let stableTokenContract:object = import_contract_from_address(self.storage.stableTokenAddr)
	let feeReceiver = self.storage.admin
	stableTokenContract:destoryAndTrans(from_address..","..tostring(stableTokenAmount)..","..feeReceiver..","..tostring(fee))
	
	cdc_info['stabilityFee'] = stableTokenAmount
	cdc_info['cdcId'] = cdc_id
	emit CloseCdc(json.dumps(cdc_info))
	return "OK"
end

--args: cdc_id[,wishPayStableTokenAmount,wishGetAssetAmount]
function M:liquidate(args:string)
	checkState(self)
	let parsed = parse_at_least_args(args,2,"arg format error, need format: cdc_id[,wishPayStableTokenAmount,wishGetAssetAmount]")
	let argcount = #parsed
	var wishPayStableTokenAmount:int = 0
	var wishGetAssetAmount:int = 0
	let from_address = tostring(parsed[1])
	let cdc_id = tostring(parsed[2])
	
	if argcount==4 then
		wishPayStableTokenAmount = tointeger(parsed[3])
		wishGetAssetAmount = tointeger(parsed[4])
		if (wishPayStableTokenAmount<=0) or (wishGetAssetAmount<=0) then
			return error("wishPayStableTokenAmount , wishGetAssetAmount must > 0")
		end
	elseif argcount~=2 then
		return error("argcount error, arg need format: cdc_id[,wishPayStableTokenAmount,wishGetAssetAmount]")
	end
	let cdcinfo = delegate_call(get_current_contract_address(), 'getLiquidableInfo', cdc_id)
	--let cdcinfo = self:getLiquidableInfo(cdc_id)
	if not cdcinfo then
		return error("getLiquidableInfo error")
	end
	let cdc_info = json.loads(tostring(cdcinfo))
	---{"owner":"eergr","collateralAmount":8000000,"stableTokenAmount":9000000000,"secSinceEpoch":956545355,"stabilityFee":3045}
	---{"isNeedLiquidation":true,"auctionCollateralAmount":2997,"repayStableTokenAmount":21800000,"curPrice":"7500.0","auctionPrice":"7275.0","penaltyAmount":1600000,"returnAmount":3,"isBadDebt":false}
	if not cdc_info["isNeedLiquidation"] then
		return error("not needLiquidation")
	end
	if cdc_info["isBadDebt"]==true then
		return error("bad Debt")
	end
	let repayStableTokenAmount = tointeger(cdc_info["repayStableTokenAmount"])
	let auctionCollateralAmount = tointeger(cdc_info["auctionCollateralAmount"])
	
	if (wishPayStableTokenAmount>0) and (repayStableTokenAmount>wishPayStableTokenAmount) then
		return error("wishPayStableTokenAmount < repayStableTokenAmount")
	end
	if (wishGetAssetAmount>0) and (wishGetAssetAmount>auctionCollateralAmount) then
		return error("wishGetAssetAmount > auctionCollateralAmount")
	end
	
	let stableTokenAmount = tointeger(cdc_info["stableTokenAmount"])
	--let from_address = get_from_address()
	let stableTokenContract:object = import_contract_from_address(self.storage.stableTokenAddr)
	let feeReceiver = self.storage.admin
	let feeAndPenalty = repayStableTokenAmount - stableTokenAmount
	stableTokenContract:destoryAndTrans(from_address..","..tostring(stableTokenAmount)..","..feeReceiver..","..tostring(feeAndPenalty))
	
	fast_map_set("cdc",cdc_id,nil)
	
	let collateralAsset = self.storage.collateralAsset
	var res = transfer_from_contract_to_address(from_address, collateralAsset, auctionCollateralAmount)
	if res ~= 0 then
		return error("transfer from contract to " .. from_address .. " realAmount " .. tostring(auctionCollateralAmount) .. " error, error code is " .. tostring(res))
	end
	
	let returnAmount = tointeger(cdc_info["returnAmount"])
	if returnAmount> 0 then
		let owner = tostring(cdc_info["owner"])
		res = transfer_from_contract_to_address(owner, collateralAsset, returnAmount)
		if res ~= 0 then
			return error("transfer from contract to " .. owner .. " realAmount " .. tostring(returnAmount) .. " error, error code is " .. tostring(res))
		end
	end
	
	self.storage.totalCollectedStablityFee = self.storage.totalCollectedStablityFee + tointeger(cdc_info["stabilityFee"])
	self.storage.totalLiquidationPenalty = self.storage.totalLiquidationPenalty + tointeger(cdc_info["penaltyAmount"])
	
	cdc_info["liquidator"] = from_address
	emit Liquidate(json.dumps(cdc_info))
	return "OK"
end



function M:takeBackCollateralByToken(from_address:string)
	if self.storage.state~='STOPPED' then
		return error("can't call this api before globalLiquidate")
	end
	let stableTokenContract:object = import_contract_from_address(self.storage.stableTokenAddr)
	let balance = tointeger(stableTokenContract:balanceOf(from_address))
	if balance <= 0 then
		return error("no balance")
	end
	let priceFeederContract:object = import_contract_from_address(self.storage.priceFeederAddr)
	let price = priceFeederContract:getPrice('')
	let sn_price = safemath.safenumber(price)
	
	let returnAmount = safemath.number_toint(safemath.number_div(safemath.safenumber(balance),sn_price))
	
	if returnAmount>0 then
		let collateralAsset = self.storage.collateralAsset
		let res = transfer_from_contract_to_address(from_address, collateralAsset, returnAmount)
		if res ~= 0 then
			return error("transfer from contract to " .. from_address .. " realAmount " .. tostring(returnAmount) .. " error, error code is " .. tostring(res))
		end
	end
	stableTokenContract:destoryAndTrans(from_address..","..tostring(balance)..",tt,0")

	let info = {"owner":from_address,"returnAmount":returnAmount,"destoryTokenAmount":balance}
	emit TakeBackCollateralByToken(json.dumps(info))
	
	return "OK"
end


function M:takeBackCollateralByCdc(cdcId:string)
	if self.storage.state~='STOPPED' then
		return error("can't call this api before globalLiquidate")
	end
	let cdc_info_obj = fast_map_get("cdc",cdcId)
	if cdc_info_obj == nil then
		return error("cdc not exist , cdc_id:"..cdcId)
	end
	let cdc_info = totable(json.loads(tostring(cdc_info_obj)))
	
	let fee = tointeger(delegate_call(get_current_contract_address(), 'getStabilityFee', cdcId))
	let stableTokenAmount = tointeger(cdc_info['stableTokenAmount'])
	let collateralAmount = tointeger(cdc_info['collateralAmount'])
	let sn_collateralAmount = safemath.safenumber(collateralAmount)
	let owner = tostring(cdc_info['owner'])
	
	let priceFeederContract:object = import_contract_from_address(self.storage.priceFeederAddr)
	let price = priceFeederContract:getPrice('')
	let sn_price = safemath.safenumber(price)
	let collateralAsset = self.storage.collateralAsset
	
	--let collateralFee = tointeger(safemath.number_toint(safemath.number_div(safemath.safenumber(fee),sn_price)))
	
	var returnAmount:int = tointeger(safemath.number_toint(safemath.number_minus(sn_collateralAmount,safemath.number_div(safemath.safenumber(fee+stableTokenAmount),sn_price))))
	
	if (returnAmount< 0) then
		let stableTokenContract:object = import_contract_from_address(self.storage.stableTokenAddr)
		let balance = tointeger(stableTokenContract:balanceOf(owner))
		if balance <= 0 then
			return error("can't cover debt")
		end
		returnAmount = returnAmount + tointeger(safemath.number_toint(safemath.number_div(safemath.safenumber(balance),sn_price)))
		if returnAmount<0 then
			return error("can't cover debt too")
		end
		stableTokenContract:destoryAndTrans(owner..","..tostring(balance)..",tt,0")
	end
	
	fast_map_set("cdc",cdcId,nil)
	if returnAmount>0 then
		let res = transfer_from_contract_to_address(owner, collateralAsset, returnAmount)
		if res ~= 0 then
			return error("transfer from contract to " .. owner .. " realAmount " .. tostring(returnAmount) .. " error, error code is " .. tostring(res))
		end
	end
	--if collateralFee > 0 then
	--	let feeReceiver = self.storage.admin
	--	let res2 = transfer_from_contract_to_address(feeReceiver, collateralAsset, collateralFee)
	--	if res2 ~= 0 then
	--		return error("transfer from contract to " .. feeReceiver .. " realAmount " .. tostring(collateralFee) .. " error, error code is " .. tostring(res2))
	--	end
	--end
	let info = {"cdcId":cdcId,"owner":owner,"returnAmount":returnAmount}
	emit TakeBackCollateralByCdc(json.dumps(info))
	return "OK"
end


function M:on_destroy()
    error("can't destroy cdc proxy contract")
end

return M



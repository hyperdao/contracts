-- contract for feed prices of different anchor tokens

-- 喂价人可以有多个，最终喂价为每个喂价人的最新喂价掐头去尾取均值（如果超过5个喂价人），如果不超过5个喂价人，最终喂价是各喂价人的最新喂价的均值

type Storage = {
    owner: string,
	state:string,
	baseAsset:string,
	maxChangeRatio:string,
	quotaAsset:string,
	price:string,  --- baseAsset/quotaAsset   baseAsset以quotaAsset计价的价格
    feeders: Array<string>, -- 喂价人地址列表
	feedPrices:Array<string>  -- 与feeders同顺序
    
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

let function checkOwner(self: table)
    if self.storage.owner ~= caller_address then
        return error("you are not owner of price feeder contract, can't call this function")
    end
end

let function getArrayIdx(array:table,feeder:string,errormsg:string)
	let count = #array
	var idx=1
	while idx<=count do
		if array[idx]==feeder then
			return idx
		end
		idx=idx+1
	end
	return error(errormsg)
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

let function checkAddress(addr: string)
    let result = is_valid_address(addr)
    if not result then
        error("address format error")
        return ""
    end
    return result
end

function M:on_deposit_asset(arg: string)
    return error("not supported deposit to price feeder contract")
end

function M:change_owner(new_owner: string)
    checkOwner(self)
    checkAddress(new_owner)
    if self.storage.owner == new_owner then
        return error("new owner address can't be same with the old one")
    end
    self.storage.owner = new_owner
    emit OwnerChanged(new_owner)
end


let function meanOfArray(array: Array<object>)
    let count = #array
	var idx = 1
	var sum:object = safemath.safenumber(0)
	if count<1 then
		return nil
	end
	while idx<=count do
		sum = safemath.number_add(array[idx],sum)
		idx = idx+1
	end
    let result = safemath.number_tostring(safemath.number_div(sum,count))
    return result
end

let function checkState(M: table)
    if M.storage.state ~= 'COMMON' then
        return error("state error, now state is " .. tostring(M.storage.state))
    end
end

let function getNewSortArray(array: Array<string>)
	let newArray:Array<object> = []
	let count = #array
	var idx = 1
	while idx<=count do
		let rec = safemath.safenumber(array[idx])
		if idx==1 then
			newArray[1]=rec
		else
			var j=1
			while j<=idx-1 do
				if(safemath.number_lte(rec,newArray[j])) then
					table.insert(newArray,j,rec)
					break
				end
				j=j+1
			end
			if #newArray < idx then
				table.append(newArray,rec)
			end
		end
		idx = idx+1
	end
	return newArray
end


-- 最终喂价为每个喂价人的最新喂价掐头去尾取均值（如果超过5个喂价人），如果不超过5个喂价人，最终喂价是各喂价人的最新喂价的均值
let function calculatePrice(feedPrices: Array<string>)
    if #feedPrices < 1 then
        return nil
    end
	let newSortArray = getNewSortArray(feedPrices)
    var i: int
	let count = #feedPrices
    if count <= 5 then
        -- 用均值作为最新喂价
        let result1 = meanOfArray(newSortArray)
        return result1
    end
    -- 掐头去尾再计算均值
    table.remove(newSortArray, count)
    table.remove(newSortArray, 1)
    let result2 = meanOfArray(newSortArray)
    return result2
end

let function arrayContains(col: Array<string>, item: string)
    if not item then
        return false
    end
	let count = #col
	var idx = 1
	while idx<=count do
		if col[idx] == item then
            return true
        end
		idx=idx+1
	end
    return false
end

---------------------------------------------------------------------------

function M:init()
    self.storage.owner = caller_address
	self.storage.state = 'NOT_INITED'
	self.storage.baseAsset = ''
	self.storage.maxChangeRatio = ''
	self.storage.quotaAsset = ''
    self.storage.feeders = []
    self.storage.feedPrices = []
    self.storage.price = ''
end


--- args: baseAsset,quotaAsset,init_price,maxChangeRatio
function M:init_config(arg:string)
	checkOwner(self)
	if self.storage.state ~= 'NOT_INITED' then
		return error("already inited ")
	end
	let parsed:Array<string> = totable(parse_args(arg,4,"arg format wrong,need format:baseAsset,quotaAsset,init_price,maxChangeRatio"))
	let pricestr = tostring(parsed[3])
	let sn_0 = safemath.safenumber(0)
	if safemath.number_lte(safemath.safenumber(pricestr),sn_0) then
		return error("price must > 0")
	end
	let maxChangeRatioStr = tostring(parsed[4])
	let sn_maxChangeRatio = safemath.safenumber(maxChangeRatioStr)
	if safemath.number_lte(sn_maxChangeRatio,sn_0) then
		return error("maxChangeRatio must > 0")
	end
	
	self.storage.baseAsset = parsed[1]
	self.storage.quotaAsset = parsed[2]
    self.storage.feeders = [self.storage.owner]
    self.storage.feedPrices = [pricestr]
    self.storage.price = pricestr
	self.storage.maxChangeRatio = maxChangeRatioStr
	self.storage.state = 'COMMON'
	emit Inited(arg)
	return "OK"
end


function M:add_feeder(new_feeder: string)
    checkOwner(self)
	checkState(self)
    checkAddress(new_feeder)
    let feeders = self.storage.feeders
	
	if arrayContains(feeders,new_feeder) then
		return error("new feeder address can't be in the feeders list")
	end
	let feedPrices = self.storage.feedPrices
    table.append(feeders, new_feeder)
	table.append(feedPrices,self.storage.price)
	---self.storage.price = calculatePrice(feedPrices)  ---- don't change???????
	self.storage.feedPrices = feedPrices
	self.storage.feeders = feeders
	
    emit FeederAdded(new_feeder)
	return "OK"
end

function M:remove_feeder(to_remove_feeder: string)
    checkOwner(self)
	checkState(self)
	let feeders = self.storage.feeders
	let feedPrices = self.storage.feedPrices
	if #feeders <=1 then
		return error("only left one feeder, can't remove")
	end
	let feedidx = tointeger(getArrayIdx(feeders,to_remove_feeder,"you are not feeder!"))	
	table.remove(feedPrices,feedidx)
	table.remove(feeders,feedidx)
	self.storage.feedPrices = feedPrices
	self.storage.feeders = feeders
	self.storage.price = tostring(calculatePrice(feedPrices))
	emit FeederRemoved(to_remove_feeder)
	return "OK"
end


-- 喂价
function M:feed_price(priceStr: string)
	checkState(self)
	let from_address = get_from_address()
	let feedidx = tointeger(getArrayIdx(self.storage.feeders,from_address,"you are not feeder!"))

	let feedPrices:Array<string> = self.storage.feedPrices
	let originalPriceStr = feedPrices[feedidx]
	let sn_price = safemath.safenumber(priceStr)
	let sn_originalPrice = safemath.safenumber(originalPriceStr)
	let sn_maxChangeRatio = safemath.safenumber(self.storage.maxChangeRatio)
	
	if(safemath.number_gt(safemath.number_div(safemath.number_abs(safemath.number_minus(sn_price,sn_originalPrice)),sn_originalPrice),sn_maxChangeRatio)) then
		return error("exceed max change ratio:"..(self.storage.maxChangeRatio))
	end
	
	feedPrices[feedidx] = priceStr
	self.storage.price = tostring(calculatePrice(feedPrices))
	let info = {"feeder":from_address,"price":priceStr}
    emit PriceFeeded(json.dumps(info))
	return "OK"
end


offline function M:owner(_: string)
	let owner = self.storage.owner
    return owner
end

offline function M:feeders(_: string)
	checkState(self)
    let feeders = self.storage.feeders
    let feedersStr = json.dumps(feeders)
    return feedersStr
end

offline function M:feedPrices(_: string)
	checkState(self)
    let feedPricesStr = json.dumps(self.storage.feedPrices)
    return feedPricesStr
end

offline function M:getPrice(_: string)
	checkState(self)
    let priceStr = self.storage.price
    return priceStr
end

offline function M:baseAsset(_: string)
	checkState(self)
    let r = self.storage.baseAsset
    return r
end

offline function M:quotaAsset(_: string)
	checkState(self)
    let r = self.storage.quotaAsset
    return r
end

offline function M:state(_: string)
    let r = self.storage.state
    return r
end

function M:on_destroy()
    error("can't destroy price feeder contract")
end

return M
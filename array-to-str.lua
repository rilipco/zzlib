
local unpack = table.unpack or unpack

-- The maximum number of table elements to unpack in one step
local MAX_UNPACK = 2048

local function array_to_str(array, start, ending)
  start = start or 1
  ending = ending or #array
  local tmp = {}
  local size = ending - start + 1
  local pos = start
  local imax = 1
  while size > 0 do
    -- Limit unpack amount to MAX_UNPACK many elements
    local bsize = size>=MAX_UNPACK and MAX_UNPACK or size
    local s = string.char(unpack(array,pos,pos+bsize-1))
    pos = pos + bsize
    size = size - bsize
    local i = 1
    while tmp[i] do
      s = tmp[i]..s
      tmp[i] = nil
      i = i + 1
    end
    if i > imax then
      imax = i
    end
    tmp[i] = s
  end
  local str = ""
  for i=1,imax do
    if tmp[i] then
      str = tmp[i]..str
    end
  end
  return str
end

return array_to_str
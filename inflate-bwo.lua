
-- zzlib - zlib decompression in Lua - version using Lua 5.3 bitwise operators

-- Copyright (c) 2016-2022 Francois Galea <fgalea at free.fr>
-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- the COPYING file or http://www.wtfpl.net/ for more details.


local inflate = {}

local array_to_str = require("array_to_str")

-- The soft limit for the decompression buffer.
--
-- If more that that many bytes are currently decompressed, a chunk will be
-- written to the output.
--
-- Must be above 32 k, because the decompression requires a buffer of at least
-- 32 KiB for back-references.
local DECOMPRESSION_BUFFER_SIZE = 64 * 1024

function inflate.band(x,y) return x & y end
function inflate.rshift(x,y) return x >> y end

function inflate.bitstream_init(file)
  local bs = {
    file = file,  -- the open file handle
    buf = nil,    -- character buffer
    len = nil,    -- length of character buffer
    pos = 1,      -- position in char buffer, next to be read
    b = 0,        -- bit buffer
    n = 0,        -- number of bits in buffer
  }
  -- get rid of n first bits
  function bs:flushb(n)
    self.n = self.n - n
    self.b = self.b >> n
  end
  -- returns the next byte from the stream, excluding any half-read bytes
  function bs:next_byte()
    if self.pos > self.len then
      self.buf = self.file:read(4096)
      self.len = self.buf:len()
      self.pos = 1
    end
    local pos = self.pos
    self.pos = pos + 1
    return self.buf:byte(pos)
  end
  -- peek a number of n bits from stream
  function bs:peekb(n)
    while self.n < n do
      self.b = self.b + (self:next_byte()<<self.n)
      self.n = self.n + 8
    end
    return self.b & ((1<<n)-1)
  end
  -- get a number of n bits from stream
  function bs:getb(n)
    local ret = bs:peekb(n)
    self.n = self.n - n
    self.b = self.b >> n
    return ret
  end
  -- get next variable-size of maximum size=n element from stream, according to Huffman table
  function bs:getv(hufftable,n)
    local e = hufftable[bs:peekb(n)]
    local len = e & 15
    local ret = e >> 4
    self.n = self.n - len
    self.b = self.b >> len
    return ret
  end
  function bs:close()
    if self.file then
      self.file:close()
    end
  end
  if type(file) == "string" then
    bs.file = nil
    bs.buf = file
  else
    bs.buf = file:read(4096)
  end
  bs.len = bs.buf:len()
  return bs
end

local function hufftable_create(depths)
  local nvalues = #depths
  local nbits = 1
  local bl_count = {}
  local next_code = {}
  for i=1,nvalues do
    local d = depths[i]
    if d > nbits then
      nbits = d
    end
    bl_count[d] = (bl_count[d] or 0) + 1
  end
  local table = {}
  local code = 0
  bl_count[0] = 0
  for i=1,nbits do
    code = (code + (bl_count[i-1] or 0)) * 2
    next_code[i] = code
  end
  for i=1,nvalues do
    local len = depths[i] or 0
    if len > 0 then
      local e = (i-1)*16 + len
      local code = next_code[len]
      local rcode = 0
      for j=1,len do
        rcode = rcode + ((1&(code>>(j-1))) << (len-j))
      end
      for j=0,2^nbits-1,2^len do
        table[j+rcode] = e
      end
      next_code[len] = next_code[len] + 1
    end
  end
  return table,nbits
end

local function block_loop(out,bs,nlit,ndist,littable,disttable)
  local lit
  repeat
    lit = bs:getv(littable,nlit)
    if lit < 256 then
      table.insert(out,lit)
    elseif lit > 256 then
      local nbits = 0
      local size = 3
      local dist = 1
      if lit < 265 then
        size = size + lit - 257
      elseif lit < 285 then
        nbits = (lit-261) >> 2
        size = size + ((((lit-261)&3)+4) << nbits)
      else
        size = 258
      end
      if nbits > 0 then
        size = size + bs:getb(nbits)
      end
      local v = bs:getv(disttable,ndist)
      if v < 4 then
        dist = dist + v
      else
        nbits = (v-2) >> 1
        dist = dist + (((v&1)+2) << nbits)
        dist = dist + bs:getb(nbits)
      end
      local p = #out-dist+1
      if p < 0 then error("Back reference way too far, dist: "..tostring(dist)) end
      while size > 0 do
        if not out[p] then error("Back reference out of bounds, p: "..tostring(p)) end
        table.insert(out,out[p])
        p = p + 1
        size = size - 1
      end
    end
  until lit == 256
end

local function block_dynamic(out,bs)
  local order = { 17, 18, 19, 1, 9, 8, 10, 7, 11, 6, 12, 5, 13, 4, 14, 3, 15, 2, 16 }
  local hlit = 257 + bs:getb(5)
  local hdist = 1 + bs:getb(5)
  local hclen = 4 + bs:getb(4)
  local depths = {}
  for i=1,hclen do
    local v = bs:getb(3)
    depths[order[i]] = v
  end
  for i=hclen+1,19 do
    depths[order[i]] = 0
  end
  local lengthtable,nlen = hufftable_create(depths)
  local i=1
  while i<=hlit+hdist do
    local v = bs:getv(lengthtable,nlen)
    if v < 16 then
      depths[i] = v
      i = i + 1
    elseif v < 19 then
      local nbt = {2,3,7}
      local nb = nbt[v-15]
      local c = 0
      local n = 3 + bs:getb(nb)
      if v == 16 then
        c = depths[i-1]
      elseif v == 18 then
        n = n + 8
      end
      for j=1,n do
        depths[i] = c
        i = i + 1
      end
    else
      error("wrong entry in depth table for literal/length alphabet: "..v);
    end
  end
  local litdepths = {} for i=1,hlit do table.insert(litdepths,depths[i]) end
  local littable,nlit = hufftable_create(litdepths)
  local distdepths = {} for i=hlit+1,#depths do table.insert(distdepths,depths[i]) end
  local disttable,ndist = hufftable_create(distdepths)
  block_loop(out,bs,nlit,ndist,littable,disttable)
end

local function block_static(out,bs)
  local cnt = { 144, 112, 24, 8 }
  local dpt = { 8, 9, 7, 8 }
  local depths = {}
  for i=1,4 do
    local d = dpt[i]
    for j=1,cnt[i] do
      table.insert(depths,d)
    end
  end
  local littable,nlit = hufftable_create(depths)
  depths = {}
  for i=1,32 do
    depths[i] = 5
  end
  local disttable,ndist = hufftable_create(depths)
  block_loop(out,bs,nlit,ndist,littable,disttable)
end

local function block_uncompressed(out,bs)
  bs:flushb(bs.n&7)
  local len = bs:getb(16)
  if bs.n > 0 then
    error("Unexpected.. should be zero remaining bits in buffer.")
  end
  local nlen = bs:getb(16)
  if len~nlen ~= 65535 then
    error("LEN and NLEN don't match")
  end
  for i=1,len do
    table.insert(out,bs:next_byte())
  end
end

--- Auxiliary function to write out a block of data
---
--- `output` may either be a string or an Lua IO file.
local function write_out_block(block_str, output_obj, crc_obj)
    -- Update CRC
    crc_obj.crc = inflate.crc32(block_str, crc_obj.crc)

    -- Output block, according to the output type.
    -- We only support local string caching, and Lua IO file output
    if type(output_obj.out) == "string" then
      output_obj.out = output_obj.out .. block_str
    else
      -- Just gona assume it is an IO file
      output_obj.out:write(block_str)
    end
end

--- Auxiliary function to decompress the entire `bs` using the given window
--- array.
---
--- Notice thet Deflate needs at least 32 KiB of past decompressed data for back
--- references.
---
--- When the end of the bs stream is reached, this function will terminate with
--- some data left in the window, which has not yet been written out to output.
local function process(bs, window, output_obj, crc_obj)
  if DECOMPRESSION_BUFFER_SIZE < 32 * 1024 then
    error("The decompression buffer size must be at least 32 KiB")
  end

  while true do
    -- Fill up the window until it reaches 64 KiB
    while #window < DECOMPRESSION_BUFFER_SIZE do
      -- Decode block type
      local last = bs:getb(1)
      local type = bs:getb(2)

      -- Decompress block according to its type
      if type == 0 then
        block_uncompressed(window,bs)
      elseif type == 1 then
        block_static(window,bs)
      elseif type == 2 then
        block_dynamic(window,bs)
      else
        error("unsupported block type")
      end

      -- If last block, just terminate
      if last == 1 then
        return
      end
    end

    -- Here we got our window full with over 64 KiB, while we only need 32 KiB,
    -- thus we will write out the oldest 32 KiB, and reduce the used array.

    -- Once we got to 64 KiB let us write out the oldest 32 KiB, thus preserving
    -- the newer 32 KiB that we need for back-references
    --
    -- |            window           | previouly
    -- +-- out_size --+-- new_size --+
    -- |   out_str    |    window    | there after

    local new_size = 32 * 1024 -- We need exactly 32 KiB
    local out_size = #window - new_size
    if out_size <= 0 then
      error("Invalid state")
    end

    -- We just take the first "out_size" many bytes from the array
    local out_str = array_to_str(window, 1, out_size)
    
    -- Then we have to move the upperhalf of the array down to the begining
    -- That means "new_size" many elements starting at "out_size" needed to be
    -- move back to the start of the array
    table.move(window, out_size + 1, out_size + 1 + new_size - 1, 1)

    -- Since a Lua "move" appears to be just a "copy", the old entries must be
    -- delete there after individually.
    while #window > new_size do
      table.remove(window)
    end

    -- Write out the out block
    write_out_block(out_str, output_obj, crc_obj)

    collectgarbage()
  end
end

--- Inflate the given BS into output
---
--- Output may be a string, a Lua file (e.g. from `io.open`), or nil (which
--- defaults) to an empty string.
---
--- In case output is a string (or nil), the entire output will be written into
--- that string, and returned as second return value.
---
--- In case output is a file, the decompressed data is directly streamed out to
--- that file, limiting the amount of main memory used during this operation.
---
function inflate.main(bs, output)
  -- Use inline string by default
  output = output or ""
  
  local output_obj = {out = output}

  -- The decompression window, needed for back references
  local window = {}
  -- The CRC of the decompressed data, as Lua table with fild "crc"
  -- The table is needed to have an object that can be passed around, while
  -- allowing to share it.
  local crc_obj = {}

  -- Decompress the entire input (aka bs)
  process(bs, window, output_obj, crc_obj)

  -- Stringify the rest of the window
  local win_str = string.char(table.unpack(window))

  -- Write out the very last block
  write_out_block(win_str, output_obj, crc_obj)

  bs:flushb(bs.n&7)

  -- Return the CRC and the output object
  return crc_obj.crc, output_obj.out
end

local crc32_table
function inflate.crc32(s,crc)
  if not crc32_table then
    crc32_table = {}
    for i=0,255 do
      local r=i
      for j=1,8 do
        r = (r >> 1) ~ (0xedb88320 & ~((r & 1) - 1))
      end
      crc32_table[i] = r
    end
  end
  crc = (crc or 0) ~ 0xffffffff
  for i=1,#s do
    local c = s:byte(i)
    crc = crc32_table[c ~ (crc & 0xff)] ~ (crc >> 8)
  end
  crc = (crc or 0) ~ 0xffffffff
  return crc
end

return inflate

local log = hs.logger.new('init.lua', 'debug')

function filter(func, tbl)
   local newtbl= {}
   for k,v in pairs(tbl) do
      if func(v) then
	 table.insert(newtbl, v)
      end
   end
   return newtbl
end

function map(func, tbl)
   local newtbl = {}
   for i,v in pairs(tbl) do
      newtbl[i] = func(v)
   end
   return newtbl
end

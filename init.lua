

	-- helper function for mark_min_max_height_in_mapchunk(..)
	-- math_extrema: math.min for maxheight; math.max for minheight
	-- populates the tables minheight and maxheight with data;
	local mark_min_max_height_local = function(minp, maxp, heightmap, ax, az, i, chunksize, minheight, maxheight, direction)
		i = i+1;
		if( ax==minp.x or az==minp.z or ax==maxp.x or az==maxp.z) then
			minheight[i] = heightmap[i];
			maxheight[i] = heightmap[i];
		else
			if( not( minheight[i])) then
				minheight[i] = -100000;
			end
			if( not( maxheight[i])) then
				maxheight[i] =  100000;
			end

			local i_side = i-chunksize;
			local i_prev = i-1;
			local i_add  = -1;
			local swap_args = false;
			if( direction==-1 ) then
				i_side = i+chunksize;
				i_prev = i+1;
				i_add  = 1;
				swap_args = true;
			else
				direction = 1;
			end

			-- do for minheight (=search for hills)
			local hr = minheight[ i_side ];
			-- handle minheight
			-- compare minheight with the neighbour to the right or left
			if( hr and heightmap[i] and hr>minheight[i]) then
				minheight[i] = math.min(hr, heightmap[i]);
			end

			if( ((direction==1 and ax>minp.x) or (direction==-1 and ax<maxp.x))
			   -- has the neighbour before a higher minheight?
			   and minheight[ i_prev ]
			   and minheight[ i_prev ] > minheight[ i ]) then
				minheight[ i ] = math.min( minheight[ i_prev ], heightmap[i]);
			end
			hr = minheight[ i ];
			-- walk backward in that row and set all with a lower minheight but
			-- a sufficiently high height to the new minheight
			local n = 1;
			local i_run = i-n;
			while( hr
			   and ((direction==1 and (ax-n)>=minp.x) or (direction==-1 and (ax+n)<=maxp.x))
			   -- has the neighbour before a lower minheight?
			   and minheight[ i_run ]
			   and minheight[ i_run ] < hr
			   -- is the neighbour before heigh enough?
			   and (heightmap[ i_run ] >= hr or heightmap[ i_run ] > minheight[ i_run ])) do
				hr = math.min( hr, heightmap[ i_run ]);
				minheight[ i_run ] = hr;

				n = n+1;
				i_run = i_run + i_add;
			end

			-- same for maxheight (= search for holes)
			hr = maxheight[ i_side ];
			-- compare maxheight with the neighbour to the right or left
			if( hr and heightmap[i] and hr<maxheight[i]) then
				maxheight[i] = math.max(hr, heightmap[i]);
			end

			if( ((direction==1 and ax>minp.x) or (direction==-1 and ax<maxp.x))
			   -- has the neighbour before a higher maxheight?
			   and maxheight[ i_prev ]
			   and maxheight[ i_prev ] < maxheight[ i ]) then
				maxheight[ i ] = math.max( maxheight[ i_prev ], heightmap[i]);
			end
			hr = maxheight[ i ];
			-- walk backward in that row and set all with a lower maxheight but
			-- a sufficiently high height to the new maxheight
			local n = 1;
			local i_run = i-n;
			while( hr
			   and ((direction==1 and (ax-n)>=minp.x) or (direction==-1 and (ax+n)<=maxp.x))
			   -- has the neighbour before a lower maxheight?
			   and maxheight[ i_run ]
			   and maxheight[ i_run ] > hr
			   -- is the neighbour before heigh enough?
			   and (heightmap[ i_run ] <= hr or heightmap[ i_run ] < maxheight[ i_run ])) do
				hr = math.max( hr, heightmap[ i_run ]);
				maxheight[ i_run ] = hr;

				n = n+1;
				i_run = i_run + i_add;
			end
		end
	end

-- detect places where nodes might be removed or added without changing the borders
-- of the mapchunk; afterwards, the landscape may be levelled, but one hill or hole
-- cannot yet be distinguished from the other;
-- more complex shapes may require multiple runs
-- Note: There is no general merging here (apart fromm the two runs) because MT maps are
--       usually very small-scale and there would be too many areas that may need merging.
local mark_min_max_height_in_mapchunk = function(minp, maxp, heightmap)
	local chunksize = maxp.x - minp.x + 1;
	local minheight = {}
	local maxheight = {}
	for j=1, 2 do
		local i = 0
		for az=minp.z,maxp.z do
		for ax=minp.x,maxp.x do
			-- fill minheight and maxheight with data whereever hills or holes are
			mark_min_max_height_local(minp, maxp, heightmap, ax, az, i, chunksize, minheight, maxheight, 1);
			i = i+1
		end
		end

		-- we keep i the way it is;
		i = i+1;
		-- the previous run could not cover all situations; check from the other side now
		for az=maxp.z,minp.z,-1 do
		for ax=maxp.x,minp.x,-1 do
			-- update minheight and maxheight for hills and holes; but this time, start from the
			-- opposite corner of the mapchunk in order to preserve what is needed there
			mark_min_max_height_local(minp, maxp, heightmap, ax, az, i, chunksize, minheight, maxheight, -1);
			i = i-1;
		end
		end
	end
	return {minheight = minheight, maxheight = maxheight};
end


	-- helper function for mark_holes_and_hills_in_mapchunk(..)
	local identify_individual_holes_or_hills = function( minp, maxp, ax, az, i, chunksize, markmap, merge_into, hole_counter, hole_data, h_real, h_max, condition)
		markmap[ i ] = 0;
		-- no hole or hill
		if( not( condition )) then
			return hole_counter;
		end
		local h_prev_z = markmap[ i-chunksize ];
		local h_prev_x = markmap[ i-1 ];
		local match_z = 0;
		local match_x = 0;
		-- if the node to the right (at z=z-1) is also part of a hole, then
		-- both nodes are part of the same hole
		if( az>minp.z and h_prev_z and h_prev_z > 0 ) then
			match_z = h_prev_z;
		end
		-- if the node before (at x=x-1) is also part of a hole, then both
		-- nodes are also part of the same hole
		if( ax>minp.x and h_prev_x and h_prev_x > 0 ) then
			match_x = h_prev_x;
		end

		-- continue the hole from z direction
		if(     match_z > 0 and match_x ==0) then
			markmap[ i ] = merge_into[ match_z ];
		-- continue the hole from x direction
		elseif( match_z ==0 and match_x > 0) then
			markmap[ i ] = merge_into[ match_x ];
		-- new hole at this place
		elseif( match_z ==0 and match_x ==0) then
			hole_counter = hole_counter + 1;
			merge_into[ hole_counter ] = hole_counter;
			markmap[ i ] = hole_counter;
		-- both are larger than 0 and diffrent - we need to merge
		else
			markmap[ i ] = merge_into[ match_z ];
			-- actually do the merge
			for k,v in ipairs(merge_into) do
				if( merge_into[ k ] == match_x ) then
					merge_into[ k ] = merge_into[ match_z ];
				end
			end
		end

		-- gather some statistical data in hole_data
		if( markmap[ i ]>0 ) then
			id = markmap[ i ];
			-- height difference
			ay = math.abs(h_max - h_real);
			if( not( hole_data[ id ])) then
				hole_data[ id ] = {
					minp = {x=ax, z=az, y=math.min(h_max, h_real)},
					maxp = {x=ax, z=az, y=math.max(h_max, h_real)},
					size = 1,
					volume = ay,
					};
			else
				-- the surface area is one larger now
				hole_data[ id ].size   = hole_data[ id ].size   + 1;
				-- the volume has also grown
				hole_data[ id ].volume = hole_data[ id ].volume + ay;
				if( ax < hole_data[ id ].minp.x ) then
					hole_data[ id ].minp.x = ax;
				end
				-- minimal and maximal dimensions may have changed
				hole_data[ id ].minp.x = math.min( ax, hole_data[ id ].minp.x );
				hole_data[ id ].maxp.x = math.max( ax, hole_data[ id ].maxp.x );
				hole_data[ id ].minp.z = math.min( az, hole_data[ id ].minp.z );
				hole_data[ id ].maxp.z = math.max( az, hole_data[ id ].maxp.z );
				hole_data[ id ].minp.y = math.min( ay, hole_data[ id ].minp.y );
				hole_data[ id ].maxp.y = math.max( ay, hole_data[ id ].maxp.y );
			end
		end
		return hole_counter;
	end


	-- helper function for mark_holes_and_hills_in_mapchunk(..)
	-- works the same for hills and holes
	local merge_if_same_hole_or_hill = function(hole_data, merge_into)
		local id2merged = {}
		local merged = {}
		hole_counter = 1;
		-- we already know from merge_into that k needs to be merged into v
		for k,v in ipairs(merge_into) do
			-- we have not covered the merge target
			if( not( id2merged[ v ])) then
				id2merged[ v ] = hole_counter;
				hole_counter = hole_counter + 1;
				merged[ v ] = hole_data[ v ];
			-- another hole or hill has already been treated -> merge with new data needed
			else
				-- merge hole_data_merged
				merged[v].size   = merged[ v ].size   + hole_data[ k ].size;
				merged[v].volume = merged[ v ].volume + hole_data[ k ].volume;
				-- minimal and maximal dimensions may have changed
				merged[v].minp.x = math.min( merged[v].minp.x, hole_data[k].minp.x );
				merged[v].maxp.x = math.max( merged[v].maxp.x, hole_data[k].maxp.x );
				merged[v].minp.z = math.min( merged[v].minp.z, hole_data[k].minp.z );
				merged[v].maxp.z = math.max( merged[v].maxp.z, hole_data[k].maxp.z );
				merged[v].minp.y = math.min( merged[v].minp.y, hole_data[k].minp.y );
				merged[v].maxp.y = math.max( merged[v].maxp.y, hole_data[k].maxp.y );
			end
			id2merged[ k ] = id2merged[ v ];
		end
		return {id2merged=id2merged, merged=merged};
	end


local mark_holes_and_hills_in_mapchunk = function( minp, maxp, heightmap, minheight, maxheight)
	local chunksize = maxp.x - minp.x + 1;
	-- distinguish the individual hills and holes from each other so that we may treat
	-- each one diffrently if so desired
	local holes_markmap = {}
	local hills_markmap = {}
	-- used to mark the individual holes on the markmap
	local hole_counter = 0;
	local hill_counter = 0;
	-- some holes will first be seen from diffrent directions and get diffrent IDs (=
	-- hole_counter) assigned; these need to be merged because they're the same
	local holes_merge_into = {};
	local hills_merge_into = {};
	-- store size, minp/maxp, max/min depth/height
	local hole_data = {};
	local hill_data = {};

	local i = 0
	for az=minp.z,maxp.z do
	for ax=minp.x,maxp.x do
		i = i+1;

		local h_real = heightmap[i];
		local h_min  = minheight[i];
		local h_max  = maxheight[i];
		-- do this for holes
		hole_counter = identify_individual_holes_or_hills( minp, maxp, ax, az, i, chunksize,
			holes_markmap, holes_merge_into, hole_counter, hole_data, h_real, h_min,
			-- h_max>0 because we do not want to create pools/fill land below sea level
			( h_max and h_real and h_max>h_real and h_max<maxp.y and h_max>minp.y and h_max>0));
		-- ..and for hills
		hill_counter = identify_individual_holes_or_hills( minp, maxp, ax, az, i, chunksize,
			hills_markmap, hills_merge_into, hill_counter, hill_data, h_real, h_max,
			-- the socket of individual hills may well lie below water level
			( h_min and h_real and h_min<h_real and h_min<maxp.y and h_min>minp.y and h_min>minp.y));
	end
	end

	-- a hole or hill might have been found from diffrent directions and thus
	-- might have gotten diffrent ids; merge them if they represent the same
	-- hole or hill
	holes = merge_if_same_hole_or_hill(hole_data, holes_merge_into);
	hills = merge_if_same_hole_or_hill(hill_data, hills_merge_into);

	return {holes = holes, holes_merge_into = holes_merge_into, holes_markmap = holes_markmap,
	        hills = hills, hills_merge_into = hills_merge_into, hills_markmap = hills_markmap};
end



minetest.register_on_generated(function(minp, maxp, seed)

	local heightmap = minetest.get_mapgen_object('heightmap');

	local t1 = minetest.get_us_time();
	local chunksize = maxp.x - minp.x + 1;

	extrema = mark_min_max_height_in_mapchunk(minp, maxp, heightmap);
	hills_and_holes = mark_holes_and_hills_in_mapchunk( minp, maxp, heightmap, extrema.minheight, extrema.maxheight);
	local holes = hills_and_holes.holes;
	local hills = hills_and_holes.hills;
	local holes_merge_into = hills_and_holes.holes_merge_into;
	local hills_merge_into = hills_and_holes.hills_merge_into;
	local holes_markmap = hills_and_holes.holes_markmap;
	local hills_markmap = hills_and_holes.hills_markmap;

	local t2 = minetest.get_us_time();
	print("Time elapsed: "..tostring( t2-t1 ));


	-- this is just for visualization
	materials = {"default:fence_wood", "default:fence_junglewood",
		 "default:fence_pine_wood","default:fence_acacia_wood",
		 "default:fence_aspen_wood", "default:glass", "default:obsidian_glass",
		 "default:ladder_wood", "default:ladder_steel",
		 "default:sign_wall_wood", "default:sign_wall_steel",
		 "doors:trapdoor", "doors:trapdoor_steel",
		 "default:papyrus",
		 "flowers:mushroom_brown", "flowers:mushroom_red",
		 "flowers:dandelion_white", "flowers:rose", "flowers:geranium"};
	local next_type = 0;
	for id, data in pairs( holes.merged ) do
		if( holes.merged[id].size > 5 ) then
			next_type = next_type+1;
			if( next_type < #materials ) then
				holes.merged[id].material = materials[ next_type ];
			else
				holes.merged[id].material = "default:brick";
			end
		else
			holes.merged[id].material = "default:meselamp";
		end
		if( not( holes.merged[id].material )) then
			holes.merged[id].material = "default:mese";
		end
		-- print("Hole "..tostring(id).." gets "..tostring(holes.merged[id].material))
	end

	materials = {"wool:white", "wool:yellow", "wool:orange", "wool:red", "wool:magenta",
		     "wool:blue", "wool:black", "wool:brown", "wool:grey"}
	next_type = 0;
	for id, data in pairs( hills.merged ) do
		if( hills.merged[id].size > 5 ) then
			next_type = next_type+1;
			if( next_type < #materials ) then
				hills.merged[id].material = materials[ next_type ];
			else
				hills.merged[id].material = "default:stonebrick";
			end
		else
			hills.merged[id].material = "default:sandstonebrick";
		end
		if( not( hills.merged[id].material )) then
			hills.merged[id].material = "default:mese";
		end
		-- print("Hill "..tostring(id).." gets "..tostring(hills.merged[id].material))
	end


	-- show something to the user; change the landscape
	local i = 0
	for az=minp.z,maxp.z do
	for ax=minp.x,maxp.x do
		i = i+1;
		if( ax==minp.x or ax==maxp.x or az==minp.z or az==maxp.z) then
			minetest.set_node({ x=ax, z=az, y=heightmap[i]}, {name="wool:pink"});
		else
			if( hills_markmap[i] and hills_markmap[i]>0) then
				id = hills_merge_into[ hills_markmap[i] ];
				if( hills.merged[ id ] and hills.merged[ id ].material) then
					for y=extrema.minheight[i]+1, heightmap[i] do
						minetest.set_node( {x=ax, z=az, y=y}, {name=hills.merged[id].material} );
					end
				else
					minetest.set_node( {x=ax, z=az, y=extrema.maxheight[i]}, {name="default:diamondblock"});
					print("Error: No material for hill id "..tostring(id)..", merged into "..tostring( holes.merged[id])) -- TODO
				end
			end
--]]
			if( holes_markmap[i] and holes_markmap[i]>0) then
				id = holes_merge_into[ holes_markmap[i] ]; --holes_merge_into[ holes_markmap[i] ]];
				if( holes.merged[ id ] and holes.merged[ id ].material) then
					minetest.set_node( {x=ax, z=az, y=extrema.maxheight[i]}, {name=holes.merged[id].material} );
				else
					minetest.set_node( {x=ax, z=az, y=extrema.maxheight[i]}, {name="default:diamondblock"});
					print("Error: No material for hole id "..tostring(id)..", merged into "..tostring( holes.merged[id])) -- TODO
				end
				-- mark holes additionally with a glass cover
				minetest.set_node( {x=ax, z=az, y=extrema.maxheight[i]+1}, {name="default:glass"});
			end
--[[
				-- highlandpools
				for y=hm+1, hv do
					minetest.set_node({ x=ax, z=az, y=y}, {name="default:water_source"});
				end
				local node = minetest.get_node( ({x=ax, z=az, y=hm}));
				if( node and node.name
				  and (node.name == "default:dirt" or node.name == "default:dirt_with_grass")) then
					minetest.set_node({ x=ax, z=az, y=hm}, {name="default:sand"});
					for h=1,4 do
						minetest.set_node({ x=ax, z=az, y=hm-h}, {name="default:clay"});
					end
				end
--]]
		end
	end
	end
end)

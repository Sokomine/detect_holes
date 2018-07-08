
minetest.register_on_generated(function(minp, maxp, seed)
	local heightmap = minetest.get_mapgen_object('heightmap');

        local i = 0;
	local ax = 0;
 	local az = 0;

	local chunksize = maxp.x - minp.x + 1;

	-- identify and mark places that are flat areas of the required size
	for az=minp.z,maxp.z do
	for ax=minp.x,maxp.x do
		i = i+1;

		local height = heightmap[ i ];
		-- fallback if no height is provided
		if( not(height)) then
			height = 0;
		end

		add_here = false
		if(heightmap[i] and heightmap[i]>2) then
			h = heightmap[i]-2;
			if( (ax>minp.x and heightmap[i-1] and h>heightmap[i-1])
                  	 or (ax<maxp.x and heightmap[i+1] and h>heightmap[i+1])
			 or (az>minp.z and heightmap[i-chunksize] and h>heightmap[i-chunksize])
			 or (az<maxp.z and heightmap[i+chunksize] and h>heightmap[i+chunksize])) then
				add_here = true
			end
		end
		if( add_here ) then
			node = minetest.get_node({x=ax, z=az, y=heightmap[i]})
			if(   node
			  and node.name
			  and node.name ~= "default:leaves"
			  and node.name ~= "default:jungleleaves") then
				if(node.name == "default:dirt_with_grass") then
					new_name = "default:bush_leaves"
--					if(minetest.registered_nodes["hedges:pine_hedge_full"]
--					   and math.random(1,7)==1) then
--						new_name = "hedges:pine_hedge_full"
--					end
					minetest.set_node({x=ax, z=az, y=heightmap[i]+2}, {name=new_name});
				elseif( node.name == "default:desert_stone") then
					new_name = "default:desert_cobble";	
				else
					new_name = "default:fence_wood";
				end
				minetest.set_node({x=ax, z=az, y=heightmap[i]+1}, {name=new_name});
			end
		end
	end
	end


end);


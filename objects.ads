-- objects.ads

-- Copyright (c) 2012, Jeremiah LaRocco jeremiah.larocco@gmail.com

-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.

-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

with Basics;

package Objects is

--	type ISect is record
--		t : Basics.Real;
--		obj : Object_ptr;
--		u, v : Basics.Real;
--		norm : Basics.Vector;
--	end record;

	type Object is abstract tagged record
		surf_ptr : Basics.Surface_ptr;
		test_it : boolean := true;
		object_id : Natural :=0;
	end record;

	type Light is tagged record
		intensity : Basics.Color;
		pos : Basics.Point;
		light_id : Natural;
	end record;

	type Light_ptr is access all Light'Class;
	
	
	type Object_ptr is access all Object'Class;

	function "="(a,b : object_ptr) return boolean;
	function "="(a,b : light_ptr) return boolean;

	procedure test_intersect(o : Object; r : Basics.ray; h : out Basics.ISect) is abstract;

	type Triangle is new Object with record
		p0, p1, p2 : Basics.point;
	end record;
	type Triangle_ptr is access all Triangle'Class;
	procedure test_intersect(t : Triangle; r : Basics.ray; h : out Basics.ISect);
	
	type Sphere is new Object with record
		r : Basics.Real;
		center : Basics.Point;
	end record;
	type Sphere_ptr is access all Sphere'Class;
	procedure test_intersect(s : Sphere; r : Basics.ray; h : out Basics.ISect);

	function get_next_id return Natural;

private
	next_id : Natural := 0;

end Objects;

-- basics.ads

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

package Basics is
	type norm_float is digits 12 range -1.0 .. 1.0;
	type Real is digits 12;
	
	type Color is record
		red : Real;
		green : Real;
		blue : Real;
	end record;
	
	type point is record
		x : Real;
		y : Real;
		z : Real;
	end record;

	type Vector is record
		xd : Real;
		yd : Real;
		zd : Real;
	end record;
	

	type Surface is record
		Kd : COlor;
		Ks : Color;
		Kr : Color;
		Ka : Color;
		Kt : Color;
		IOR : Real;
	end record;

	type Surface_ptr is access all Surface;
	
	type ISect is record
		hit : boolean := false;
		t : Real;
		hit_pt : Point;
		normal : Vector:= (0.5,0.5,0.5);
		texcoord : Point:= (0.0,0.0,0.0);
		sp: Surface_ptr;
		ray_d : Vector;
		nhits : Natural :=0;
	end record;

	type transform is array (1..4, 1..4) of Real;

	IDENTITY_TRANSFORM : constant transform := 
		(	(1.0,0.0,0.0,0.0),
			(0.0,1.0,0.0,0.0), 
			(0.0,0.0,1.0,0.0), 
			(0.0,0.0,0.0,1.0));

	function "+"(o, d : Vector) return Vector;
	function "-"(a, b : Vector) return Vector;

	function dot(a, b : Vector) return Real;
	function cross(a, b : Vector) return Vector;

	function length(v : Vector) return Real;
	function normal(v : Vector) return Vector;
	function "*"(a : Vector; b : Real) return Vector;
	function "*"(b : Real; a : Vector) return Vector;

	--function "-"(a : Point; b : Vector) return Vector;
	

	function "+"(a, b : Color) return Color;
	function "*"(a, b : Color) return Color;
	function "*"(a : Color; b : Real) return Color;
	function "*"(b : Real; a : Color) return Color;
	

	type Ray is record
		o : Point;
		d : Vector;
	end record;

	function Point_on(r : Ray; t : Real) return Point;
	function Ray_to_from(ori : Point; dest : Point) return Ray;

	procedure Put(v : Vector);
	procedure Put(v : Point);
	procedure Put(v : Color);
end Basics;

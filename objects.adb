with Ada.Numerics.Generic_Elementary_Functions;

with Basics; use Basics;
package body Objects is

	package Math is new Ada.Numerics.Generic_Elementary_Functions(Real);
	use Math;
	--function test_intersect(o : Object; r : Basics.ray) return Long_Float is
	
	function "="(a,b : object_ptr) return boolean is
	begin
		return (a.object_id = b.object_id);
	end "=";
	
	function "="(a,b : light_ptr) return boolean is
	begin
		return (a.light_id = b.light_id);
	end "=";

	procedure test_intersect(t : Triangle; r : Basics.ray; h : out ISect) is
		e1 : Vector := (t.p1.x-t.p0.x, t.p1.y-t.p0.y, t.p1.z-t.p0.z);
		e2 : Vector := (t.p2.x-t.p0.x, t.p2.y-t.p0.y, t.p2.z-t.p0.z);
		p : Vector := cross(r.d, e2);
		a : Real := dot(e1, p);
		eps : constant Real := 0.00000007;
		f : Real;
		s : Vector;
		u : Real;
		v : Real;
		q : Vector;
	begin
		if ((a > -eps) and (a < eps)) then
			h.hit := false;
		else
			f := 1.0/a;
			s := (r.o.x, r.o.y, r.o.z) - (t.p0.x, t.p0.y, t.p0.z);
			u := f * dot(s, p);
			if ((u < 0.0) or (u > 1.0)) then
				h.hit := false;
			else
				q := cross(s, e1);
				v := f*dot(r.d, q);
				if ((v < 0.0) or ((u+v)>1.0)) then
					h.hit := false;
				else
					h.t := f*dot(e2, q);
					h.hit := true;
					h.sp := t.surf_ptr;
					h.hit_pt := Point_on(r, h.t);
					h.normal := normal(cross((t.p2.x, t.p2.y, t.p2.z), (t.p1.x, t.p1.y, t.p1.z)));
					h.ray_d := r.d;
				end if;
			end if;
		end if;
	end test_intersect;

	--	This finds the intersection of a sphere and ray using the
	--	geometric method.
	procedure test_intersect(s : Sphere; r : Basics.ray; h : out ISect) is
		sc : Vector := (s.center.x, s.center.y, s.center.z);
		OC : Vector := sc- (r.o.x, r.o.y, r.o.z);
		l2oc : Real := dot(oc, oc);
		tca, t2hc : Real;
		t1, t2 : Real;
		temp : Real;
		eps : constant Real := 0.00000007;
	begin
		if (l2oc >= s.r*s.r) then
			tca := dot(oc, r.d);
			if (tca > 0.0) then
				t2hc := s.r*s.r-l2oc+tca*tca;
				if (t2hc>0.0) then
					temp := sqrt(t2hc);
					t1 := tca - temp;
					t2 := tca+temp;
					h.hit := true;
					h.sp := s.surf_ptr;
					h.t := Real'max(t1,t2);
					h.hit_pt := Point_on(r, h.t);
					h.normal := normal(( h.hit_pt.x-s.center.x,
												h.hit_pt.y-s.center.y,
												h.hit_pt.z-s.center.z));
					h.ray_d := r.d;
					
				else
					h.hit := false;
				end if;
			else
				h.hit := false;
			end if;
		else
			h.hit := false;
		end if;
	end test_intersect;

	function get_next_id return natural is
	begin
		next_id := next_id+1;
		return next_id-1;
	end get_next_id;

end Objects;

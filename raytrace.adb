-- raytrace.adb

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

with Basics; use Basics;
with Scene;     use Scene;
with Objects;   use Objects;
with grid;
use grid;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Numerics.Generic_elementary_functions;


package body RayTrace is
   package Math is new Ada.Numerics.Generic_elementary_functions(Real);
   use Math;

   package FIO is new Ada.Text_IO.Float_IO(Real);
   use FIO;
   package IIO is new Ada.Text_IO.Integer_IO(Natural);
   use IIO;

   procedure shade(s : in out Scene.Scene; ist : ISect; retval : out Basics.Color) is
      --        This code is modified from Glassner's book.
      --        I assume:
      --                Fdr = 1
      --                I1j = the_light.intensity
      --
      shade_ist : ISect;
      shade_ray : Ray;

      reflect_ray : Ray;
      reflect_ist : ISect;

      refract_ray : Ray;
      refract_ist : ISect;
      costt : Real;
      Ci : Real;


      the_light : Light_ptr;
      col : Color;
      Kdfact : Color := (0.0,0.0,0.0);
      Ksfact : Color := (0.0,0.0,0.0);
      Krfact : Color := (0.0,0.0,0.0);
      Ktfact : Color := (0.0,0.0,0.0);
      n : Real := 2.0;
      depth : Natural := 5;
      --temp : Vector;
      tempc : Color;
   begin
      if (num_lights(s)<1) then
         Put_Line("No lights in scene.");
         col := ist.sp.Ka;
      end if;
      for i in 1..num_lights(s) loop
         shade_ray := get_light_ray(s, i, ist.hit_pt);
         shade_ist := trace(s, shade_ray);
         if ( shade_ist.hit) then
            --  In shadow
            col := ist.sp.Ka;
         else
            the_light := get_light(s, i);
            n := dot(ist.normal, shade_ray.d);
            if (n>0.0) then
               KdFact := KdFact+the_light.intensity * n;
               n:=n*n;
               n:=n*n;
               n:=n*n;
               n:=n*n;
               n:=n*n;
               Ksfact := Ksfact+ the_light.intensity * n;
               if ((ist.sp.Kr.red>0.0) and (ist.sp.Kr.green>0.0) and (ist.sp.Kr.blue>0.0) and (current_depth(s)<50)) then
                  reflect_ray.d := (-1.0)*ist.ray_d
                    +(-2.0)
                    *dot(ist.normal,(-1.0)*ist.ray_d)
                    *ist.normal;
                  reflect_ray.o := ist.hit_pt;
                  inc_depth(s);
                  reflect_ist := trace(s, reflect_ray);
                  if (reflect_ist.hit) then
                     shade(s, reflect_ist, tempc);
                  else
                     tempc := Background(s);
                  end if;
                  Krfact := Krfact+tempc;
                  dec_depth(s);
               end if;
               if ((ist.sp.Kt.red>0.0)
                   and (ist.sp.Kt.green>0.0)
                   and (ist.sp.Kt.blue>0.0)
                   and (current_depth(s)<50)) then
                  Ci := dot(ist.normal, ist.ray_d);
                  --T = ist.ray_d + [Ci - sqrt((1 + hit^2(Ci^2 - 1))^.5] N

                  costt := sqrt(abs(1.0+(ist.sp.IOR*ist.sp.ior)*(Ci*Ci-1.0)));

                  refract_ray.d := ist.sp.IOR*ist.ray_d +(Ci-costt)*ist.normal;
                  refract_ray.o := ist.hit_pt;
                  inc_depth(s);
                  refract_ist := trace(s, refract_ray);
                  if (refract_ist.hit) then
                     shade(s, refract_ist, tempc);
                  else
                     tempc := Background(s);
                  end if;
                  Ktfact := Ktfact+tempc;
                  dec_depth(s);
               end if;

            end if;
         end if;
      end loop;
      retVal := ist.sp.Ka
        + ist.sp.Kd*Kdfact
        + ist.sp.Ks*Ksfact
        + ist.sp.Kr*Krfact
        + ist.sp.Kt*Ktfact;
   end shade;

   function trace(s : Scene.Scene; r : Basics.Ray) return Basics.ISect is
      new_hit : ISect;
      old_hit : ISect;
      eps : Real := 0.000000001;

      xn, yn, zn : Natural; --  current grid cell
      ig : boolean;
      isectf : boolean := false;
      sg : grid_ptr := sgrid(s);

      x,y,z : Real;
      dx, dy, dz : Real;
      exy, exz, ezy : Real;

   begin
      old_hit.t := Real'Last;
      if (num_objects(s) = 0) then
         new_hit.hit := false;
         return new_hit;
      end if;

      --        This is commented out so that the grid method can be used (down below)
      --                for i in 1..(num_objects(s)) loop
      --                        test_intersect(object_num(s, i), r, new_hit);
      --
      --                        if (new_hit.hit) then
      --                                if (new_hit.t < (old_hit.t-eps)) then
      --                                        old_hit.t := new_hit.t;
      --                                        old_hit.hit := new_hit.hit;
      --                                        old_hit.sp := new_hit.sp;
      --                                        old_hit.normal := new_hit.normal;
      --                                        old_hit.hit_pt := new_hit.hit_pt;
      --                                        old_hit.ray_d := new_hit.ray_d;
      --                                end if;
      --                        end if;
      --                end loop;

      ig := true;
      isectf := false;

      x := r.o.x;
      y := r.o.y;
      z := r.o.z;

      cell_of(sg, x, y, z, xn,yn,zn, ig);

      dx := (Real(xn)*sg.xdiff + sg.xmin)-x;
      dy := (Real(yn)*sg.ydiff + sg.ymin)-y;
      dz := (Real(zn)*sg.zdiff + sg.zmin)-z;

      exy := dx*r.d.yd-dy*r.d.xd;
      exz := dx*r.d.zd-dz*r.d.xd;
      ezy := dz*r.d.yd-dy*r.d.zd;

      while (ig and not isectf) loop


         --     Check if list is populated.

         if (OList.size(sg.gp(xn,yn,zn).obj_list)>0) then
            --  Perform isect test on each element.
            for i in 1..OList.size(sg.gp(xn,yn,zn).obj_list) loop
               test_intersect(OList.at_index(cell_at(sg,xn,yn,zn).obj_list, i).all, r, new_hit);

               if (new_hit.hit) then
                  isectf := true;
                  if (new_hit.t < (old_hit.t-eps)) then
                     old_hit := new_hit;
                  end if;
               end if;
            end loop;
         end if;
         --     Now go to next cell.
         if (exy <  0.0) then
            if (exz <  0.0) then
               if (r.d.xd <0.0) then
                  x := x-sg.xdiff;
               else
                  x :=x+sg.xdiff;
               end if;
               exy := exy + abs(r.d.yd);
               exz := exz + abs(r.d.zd);
            else
               if (r.d.zd < 0.0) then
                  z := z-sg.zdiff;
               else
                  z := z+sg.zdiff;
               end if;
               exz := exz - abs(r.d.xd);
               ezy := ezy + abs(r.d.yd);
            end if;
         else
            if (ezy < 0.0) then
               if (r.d.zd < 0.0) then
                  z := z-sg.zdiff;
               else
                  z :=z+sg.zdiff;
               end if;
               exz := exz - abs(r.d.xd);
               ezy := ezy + abs(r.d.yd);
            else
               if (r.d.yd < 0.0) then
                  y := y-sg.ydiff;
               else
                  y :=y+sg.ydiff;
               end if;
               exy := exy - abs(r.d.xd);
               ezy := ezy - abs(r.d.zd);
            end if;
         end if;
         cell_of(sg, x, y, z, xn,yn,zn,ig);
      end loop;
      return old_hit;
   end trace;
end RayTrace;


-- scene.adb

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

with Objects; use Objects;
with Basics; use Basics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with InputBuffer;
use INputBuffer;
with GenList;
with grid;
use grid;


with Ada.Text_IO;
use Ada.Text_IO;

package body scene is
   package Math is new Ada.Numerics.Generic_elementary_Functions(Real);
   use Math;

   package IIO is new Ada.Text_IO.Integer_IO(Natural);
   use IIO;

   package RIO is new Ada.Text_IO.Float_IO(Real);
   use RIO;

   function To_Un(Source : in String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

   function num_objects(s :scene) return Natural is
   begin
      return s.num_objects;
   end num_objects;

   procedure read_Scene(s : in out Scene; fileName : String) is

      function read( fn : String) return Scene is
         inf : File_type;
         ib : Input_Buffer;
         curWord : Unbounded_String;
         scn : Scene;

         function get_word(ib : Input_Buffer) return Unbounded_String is
            cc : Character;
            wrd : Unbounded_String := To_Unbounded_String("");
         begin
            loop
               cc := Next_character(ib);
               exit when ((cc /=' ') and (cc/=ASCII.HT) and (cc/=ASCII.LF) and (cc/=ASCII.CR));
            end loop;
            loop
               wrd := wrd & cc;
               cc := Next_Character(ib);
               exit when ((cc =' ') or (cc=ASCII.HT) or (cc=ASCII.LF) or (cc=ASCII.CR));
            end loop;
            return wrd;
         end get_word;

      begin
         Create(ib, fn);
         declare
            tempi : natural;
            tempi2 : Natural;
            tempf : Real;
            tempp : Point;
            tempv : Vector;
            tempc : Color;
            numLight : Natural := 0;
            numObj : Natural := 0;

            --  Added to initialize grid
            mx,my,mz, xm,ym,zm : Real;
            xs, ys, zs : Natural;
         begin
            --  Read number of objects in scene (n)
            --  Read number of lights in scene (m)
            --  Read camera/image info
            --  Format is:
            --  camera location direction up right background_c width height
            -- Read m lights in the form:
            --  position, color
            --  Read n objects in the form:
            --  sphere radius center Kd Ks Ka Kt Kr IOR
            -- or:
            --  tri p1 p2 p3 Kd Ks Ka Kt Kr IOR

            --  # of objects
            curWord := get_word(ib);
            Get(To_String(curWord), tempi, tempi2);
            scn.objs := new obj_array(1..tempi);
            Put_Line("Objects:"&Natural'Image(tempi));
            scn.num_objects := tempi;

            --  # lights
            curWord := get_word(ib);
            Get(To_string(curWord), tempi, tempi2);
            scn.lights := new light_array(1..tempi);
            scn.num_lights := tempi;

            curWord:= get_word(ib);
            if (curWord = To_Un("camera")) then
               --       Read cam location
               curWord := get_word(ib);
               Get(To_string(curWord), tempp.x, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempp.y, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempp.z, tempi2);

               --       read cam direction
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.xd, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.yd, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.zd, tempi2);
               scn.cam.ori := tempp;
               scn.cam.dir := normal(tempv);

               --       read cam up
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.xd, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.yd, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.zd, tempi2);
               scn.vup := tempv;

               --       read cam right
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.xd, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.yd, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempv.zd, tempi2);
               scn.vright := tempv;


               --       read background color
               curWord := get_word(ib);
               Get(To_string(curWord), tempc.red, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempc.green, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), tempc.blue, tempi2);
               scn.background_c := tempc;

               --       width
               curWord := get_word(ib);
               Get(To_String(curWord), tempi, tempi2);
               scn.width := tempi;

               --       height
               curWord := get_word(ib);
               Get(To_string(curWord), tempi, tempi2);
               scn.height := tempi;


               --       grid divisions x
               curWord := get_word(ib);
               Get(To_String(curWord), xs, tempi2);

               --       grid divisions y
               curWord := get_word(ib);
               Get(To_string(curWord), ys, tempi2);

               --       grid divisions z
               curWord := get_word(ib);
               Get(To_string(curWord), zs, tempi2);

               --       grid min
               curWord := get_word(ib);
               Get(To_string(curWord), mx, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), my, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), mz, tempi2);


               --       grid max
               curWord := get_word(ib);
               Get(To_string(curWord), xm, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), ym, tempi2);
               curWord := get_word(ib);
               Get(To_string(curWord), zm, tempi2);

               scn.gp := create_grid(xs,ys,zs,mx,my,mz,xm,ym,zm);

            else
               Put_Line("Invalid scene file.");
            end if;

            curWord := get_word(ib);
            if (curWord = To_Un("lights")) then
               Put_Line("Beginning to read lights.");
               for i in 1.. scn.num_lights loop
                  --    read light color
                  curWord := get_word(ib);
                  Get(To_string(curWord), tempc.red, tempi2);
                  curWord := get_word(ib);
                  Get(To_string(curWord), tempc.green, tempi2);
                  curWord := get_word(ib);
                  Get(To_string(curWord), tempc.blue, tempi2);

                  --    Read light position
                  curWord := get_word(ib);
                  Get(To_string(curWord), tempp.x, tempi2);
                  curWord := get_word(ib);
                  Get(To_string(curWord), tempp.y, tempi2);
                  curWord := get_word(ib);
                  Get(To_string(curWord), tempp.z, tempi2);

                  scn.lights(i) := new light'(tempc, tempp, get_next_id);
               end loop;
            else
               Put_Line("Invalid light in scene.");
            end if;

            curWord := get_word(ib);
            if (curWord = To_Un("objects")) then
               Put_Line("Beginning to read objects.");
               for i in 1.. scn.num_objects loop
                  curWord := get_word(ib);
                  if (curWord = To_un("sphere")) then

                     -- Radius
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempf, tempi2);
                     scn.objs(i) := new Sphere;
                     Sphere_Ptr(scn.objs(i)).r := tempf;

                     -- Center
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.x, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.y, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.z, tempi2);
                     Sphere_ptr(scn.objs(i)).center := tempp;
                     scn.objs(i).object_id := get_next_id;

                     -- Read surface properties
                     scn.objs(i).surf_ptr := new Surface;

                     -- Ka
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Ka := tempc;
                     -- Kd
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Kd := tempc;
                     -- Ks
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Ks := tempc;
                     -- Kt
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Kt := tempc;
                     -- Kr
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Kr := tempc;
                     --IOR
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempf, tempi2);
                     scn.objs(i).surf_ptr.IOR := tempf;
                     add(scn.gp, Sphere_ptr(scn.objs(i)));
                  elsif (curWord = To_un("tri")) then
                     scn.objs(i) := new Triangle;

                     -- p0
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.x, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.y, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.z, tempi2);
                     Triangle_ptr(scn.objs(i)).p0 := tempp;
                     -- p1
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.x, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.y, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.z, tempi2);
                     Triangle_ptr(scn.objs(i)).p1 := tempp;
                     -- p2
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.x, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.y, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempp.z, tempi2);
                     scn.objs(i).object_id := get_next_id;
                     Triangle_ptr(scn.objs(i)).p2 := tempp;


                     -- Read surface properties
                     scn.objs(i).surf_ptr := new Surface;

                     -- Ka
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Ka := tempc;
                     -- Kd
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Kd := tempc;
                     -- Ks
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Ks := tempc;
                     -- Kt
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Kt := tempc;
                     -- Kr
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.red, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.green, tempi2);
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempc.blue, tempi2);
                     scn.objs(i).surf_ptr.Kr := tempc;
                     --IOR
                     curWord := get_word(ib);
                     Get(To_string(curWord), tempf, tempi2);
                     scn.objs(i).surf_ptr.IOR := tempf;
                     add(scn.gp, Triangle_ptr(scn.objs(i)));
                  else
                     Put_Line("Invalid surface type: " & To_String(curWord));
                  end if;
               end loop;
            else
               Put_Line("Invalid scene file.");
            end if;
         exception
            when InputBuffer.end_of_file => null;
            when others => raise;
         end;
         Destroy(ib);
         return scn;
      end read;

   begin
      s := read(fileName);
   end read_scene;

   function object_num(s : Scene; n : Positive) return Object'Class is
   begin
      if (n > s.num_objects) then
         raise invalid_object;
      end if;

      return s.objs(n).all;
   end object_num;

   function get_camera(s : Scene) return Camera is
   begin
      return s.cam;
   end get_camera;

   procedure set_camera(s : in out Scene; cam : in Camera) is
   begin
      s.cam := cam;
   end set_camera;

   function get_height(s : Scene) return Positive is
   begin
      return s.height;
   end get_height;

   function get_width(s : Scene) return Positive is
   begin
      return s.width;
   end get_width;

   procedure set_height(s : in out Scene; h : Positive) is
   begin
      s.height:=h;
   end set_height;

   procedure set_width(s : in out Scene; w : Positive) is
   begin
      s.width := w;
   end set_width;

   function get_ray(s : in Scene; i,j: Natural) return Basics.Ray is
      temp : Ray;
      x : Real := (s.xmin + (s.xmax-s.xmin)*(Real(i)/Real(s.width -1)));
      y : Real := (s.ymax - (s.ymax-s.ymin)*(Real(j)/Real(s.height-1)));
   begin
      temp.o := get_origin(s.cam);
      temp.d :=normal(x*s.vright+y*s.vup + get_direction(s.cam));
      return temp;
   end get_ray;

   function num_lights(s : Scene) return Natural is
   begin
      return s.num_lights;
   end num_lights;

   function get_light(s : Scene; i : Positive) return Light_ptr is
   begin
      if (i > s.num_lights) then
         raise invalid_light;
      end if;
      return s.lights(i);
   end get_light;

   function get_light_ray(s : in scene; light_n : Positive; o : Basics.Point) return Basics.Ray is
      r : Ray;
   begin
      if (light_n>s.num_lights) then
         raise invalid_light;
      end if;
      r := ray_to_from(o, s.lights(light_n).pos);
      r.d := normal(r.d);
      return r;
   end get_light_ray;

   function get_origin(cam : Camera) return Basics.Point is
   begin
      return cam.ori;
   end get_origin;
   procedure set_origin(cam : in out camera; origin : in Basics.point) is
   begin
      cam.ori:=origin;
   end set_origin;

   function get_direction(cam : Camera) return Basics.Vector is
   begin
      return cam.dir;
   end get_direction;
   procedure set_direction(cam : in out camera; direction : in Basics.Vector) is
   begin
      cam.dir := direction;
   end set_direction;

   function Background(s : Scene) return Basics.Color is
   begin
      return s.background_c;
   end Background;

   function current_depth(s : in Scene) return natural is
   begin
      return s.curDepth;
   end current_depth;

   procedure inc_depth(s : in out Scene) is
   begin
      s.curdepth := s.curdepth+1;
   end inc_depth;

   procedure dec_depth(s : in out scene) is
   begin
      s.curdepth := s.curdepth-1;
   end dec_depth;

   function sgrid(s : Scene) return grid_ptr is
   begin
      return s.gp;
   end sgrid;

end scene;


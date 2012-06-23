-- basics.adb

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

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

package body basics is
   package Math is new Ada.Numerics.Generic_Elementary_Functions(Real);
   use Math;

   package TIO renames Ada.Text_IO;
   package RIO is new TIO.Float_IO(Real);
   use TIO;
   use RIO;

   function "+"(o, d : Vector) return Vector is
   begin
      return (o.xd+d.xd, o.yd+d.yd, o.zd+d.zd);
   end "+";

   function "+"(a : Point; b : Vector) return Point is
   begin
      return (a.x+b.xd, a.y+b.yd, a.z+b.zd);
   end "+";

   function "-"(a, b : Vector) return Vector is
   begin
      return (a.xd-b.xd, a.yd-b.yd, a.zd-b.zd);
   end "-";

   function "-"(a : Vector; b : Point) return Vector is
   begin
      return (a.xd-b.x, a.yd-b.y, a.zd-b.y);
   end "-";

   function "-"(a : Point; b : Vector) return Vector is
   begin
      return (a.x-b.xd, a.y-b.yd, a.z-b.zd);
   end "-";

   function dot(a, b : Vector) return Real is
   begin
      return (a.xd*b.xd+a.yd*b.yd+a.zd*b.zd);
   end dot;

   function cross(a, b : Vector) return Vector is
   begin
      return (xd=>(a.yd*b.zd-a.zd*b.yd), yd=>(a.zd*b.xd-a.xd*b.zd), zd=>(a.xd*b.yd-a.yd*b.xd));
   end cross;

   function length(v : Vector) return Real is
   begin
      return sqrt(v.xd*v.xd+v.yd*v.yd+v.zd*v.zd);
   end length;

   function normal(v : Vector) return Vector is
      len : Real :=length(v);
   begin
      if (len < 0.000000001) then
         return (0.0,0.0,1.0);
      end if;
      return (v.xd/len, v.yd/len, v.zd/len);
   end normal;

   function "*"(a : Vector; b : Real) return Vector is
   begin
      return (b * a.xd, b * a.yd, b*a.zd);
   end "*";

   function "*"(b : Real; a : Vector) return Vector is
   begin
      return (b * a.xd, b * a.yd, b*a.zd);
   end "*";


   function "+"(a, b : Color) return Color is
   begin
      return (a.red+b.red, a.green+b.green, a.blue+b.blue);
   end "+";
   function "*"(a, b : Color) return Color is
   begin
      return (a.red*b.red, a.green*b.green, a.blue*b.blue);
   end "*";
   function "*"(a : Color; b : Real) return Color is
   begin
      return (b * a.red, b * a.green, b*a.blue);
   end "*";

   function "*"(b : Real; a : Color) return Color is
   begin
      return (b * a.red, b * a.green, b*a.blue);
   end "*";
   procedure Put(v : vector) is
   begin
      Put("xd = ");
      Put(v.xd, exp =>0);
      Put(" yd = ");
      Put(v.yd, exp =>0);
      Put(" zd = ");
      Put(v.zd, exp =>0);
      New_Line;
   end Put;

   procedure Put(v : Point) is
   begin
      Put("x = ");
      Put(v.x, exp =>0);
      Put(" y = ");
      Put(v.y, exp =>0);
      Put(" z = ");
      Put(v.z, exp =>0);
      New_Line;
   end Put;

   procedure Put(v : Color) is
   begin
      Put("r = ");
      Put(v.red, exp =>0);
      Put(" g = ");
      Put(v.green, exp =>0);
      Put(" b = ");
      Put(v.blue, exp =>0);
      New_Line;
   end Put;

   function Point_on(r : Ray; t : Real) return Point is
   begin
      return (r.o + t*r.d);
   end Point_on;

   function Ray_to_from(ori : Point; dest : Point) return Ray is
   begin
      return (ori, normal((dest.x-ori.x, dest.y-ori.y, dest.z-ori.z)));
   end Ray_to_from;

end basics;


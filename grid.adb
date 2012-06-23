with Basics;
use Basics;
with Objects;
use objects;
with Ada.Text_IO;

with Ada.Numerics.Generic_Elementary_Functions;


package body grid is
   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Float_IO(Real);
   package NIO is new TIO.Integer_IO(Natural);
   package Math is new Ada.Numerics.Generic_Elementary_Functions(Real);

   function create_grid(xs, ys, zs : Natural;
                        xmin, ymin, zmin,
                        xmax, ymax, zmax : Basics.Real) return grid_ptr is
      temp : grid_ptr;
   begin
      temp := new grid'(new grid_arr(0..xs, 0..ys, 0..zs), xmin, xmax, ymin, ymax,
                        zmin, zmax, ((xmax-xmin)/Real(xs-1)), ((ymax-ymin)/Real(ys-1)), ((zmax-zmin)/Real(zs-1)));
      return temp;
   end create_grid;

   --   This procedure adds sph to each every cell that might contain it.
   procedure add( tg : grid_ptr; sph : sphere_ptr) is
      xc, yc, zc : Natural;
      xe, ye, ze : Natural;

      rad : Real := sph.r;
      rad2 : Real := rad * rad;


      sx : Real := sph.center.x;
      sy : Real := sph.center.y;
      sz : Real := sph.center.z;

      tb : boolean;
   begin
      if (((sx > tg.xmax) or (sy > tg.ymax) or (sz > tg.zmax))
          or ((sx<tg.xmin) or (sy < tg.ymin) or (sz < tg.zmin))) then
         --     The sphere is not in the grid, so ignore it.
         null;
      else
         xe := Natural(sph.r/tg.xdiff)+1;
         ye := Natural(sph.r/tg.ydiff)+1;
         ze := Natural(sph.r/tg.zdiff)+1;

         cell_of(tg,sx, sy,sz, xc,yc,zc,tb);
         for i in xc-xe..xc+xe loop
            for j in yc-ye..yc+ye loop
               for k in zc-ze..zc+ze loop
                  OList.insert(tg.gp(i,j,k).obj_list, Object_ptr(sph));
               end loop;
            end loop;
         end loop;
      end if;
   end add;

   procedure add( tg : grid_ptr; tri : triangle_ptr) is
      xn, yn, zn : Natural;

      ig : boolean;
      dx,dy,dz : Real;
      exy, exz, ezy : Real;
      xd, yd, zd : Real;
      x, y, z : Real;
   begin
      x := tri.p0.x;
      y := tri.p0.y;
      z := tri.p0.z;
      cell_of(tg, x, y, z, xn, yn, zn, ig);

      xd := tri.p1.x - tri.p0.x;
      yd := tri.p1.y - tri.p0.y;
      zd := tri.p1.z - tri.p0.z;

      dx := (Real(xn)*tg.xdiff + tg.xmin)-x;
      dy := (Real(yn)*tg.ydiff + tg.ymin)-y;
      dz := (Real(zn)*tg.zdiff + tg.zmin)-z;

      exy := dx*yd-dy*xd;
      exz := dx*zd-dz*xd;
      ezy := dz*yd-dy*zd;

      while (ig) loop
         OList.insert(tg.gp(xn,yn,zn).obj_list, Object_ptr(tri));
         --     Now go to next cell.
         if (exy <  0.0) then
            if (exz <  0.0) then
               if (xd <0.0) then
                  x := x-tg.xdiff;
               else
                  x :=x+tg.xdiff;
               end if;
               exy := exy + abs(yd);
               exz := exz + abs(zd);
            else
               if (zd < 0.0) then
                  z := z-tg.zdiff;
               else
                  z := z+tg.zdiff;
               end if;
               exz := exz - abs(xd);
               ezy := ezy + abs(yd);
            end if;
         else
            if (ezy < 0.0) then
               if (zd < 0.0) then
                  z := z-tg.zdiff;
               else
                  z :=z+tg.zdiff;
               end if;
               exz := exz - abs(xd);
               ezy := ezy + abs(yd);
            else
               if (yd < 0.0) then
                  y := y-tg.ydiff;
               else
                  y :=y+tg.ydiff;
               end if;
               exy := exy - abs(xd);
               ezy := ezy - abs(zd);
            end if;
         end if;


         cell_of(tg, x, y, z, xn,yn,zn,ig);
      end loop;
      x := tri.p1.x;
      y := tri.p1.y;
      z := tri.p1.z;
      cell_of(tg, x, y, z, xn, yn, zn, ig);

      xd := tri.p2.x - tri.p1.x;
      yd := tri.p2.y - tri.p1.y;
      zd := tri.p2.z - tri.p1.z;

      dx := (Real(xn)*tg.xdiff + tg.xmin)-x;
      dy := (Real(yn)*tg.ydiff + tg.ymin)-y;
      dz := (Real(zn)*tg.zdiff + tg.zmin)-z;

      exy := dx*yd-dy*xd;
      exz := dx*zd-dz*xd;
      ezy := dz*yd-dy*zd;

      while (ig) loop
         OList.insert(tg.gp(xn,yn,zn).obj_list, Object_ptr(tri));
         --     Now go to next cell.
         if (exy <  0.0) then
            if (exz <  0.0) then
               if (xd <0.0) then
                  x := x-tg.xdiff;
               else
                  x :=x+tg.xdiff;
               end if;
               exy := exy + abs(yd);
               exz := exz + abs(zd);
            else
               if (zd < 0.0) then
                  z := z-tg.zdiff;
               else
                  z := z+tg.zdiff;
               end if;
               exz := exz - abs(xd);
               ezy := ezy + abs(yd);
            end if;
         else
            if (ezy < 0.0) then
               if (zd < 0.0) then
                  z := z-tg.zdiff;
               else
                  z :=z+tg.zdiff;
               end if;
               exz := exz - abs(xd);
               ezy := ezy + abs(yd);
            else
               if (yd < 0.0) then
                  y := y-tg.ydiff;
               else
                  y :=y+tg.ydiff;
               end if;
               exy := exy - abs(xd);
               ezy := ezy - abs(zd);
            end if;
         end if;


         cell_of(tg, x, y, z, xn,yn,zn,ig);
      end loop;
      x := tri.p2.x;
      y := tri.p2.y;
      z := tri.p2.z;
      cell_of(tg, x, y, z, xn, yn, zn, ig);

      xd := tri.p0.x - tri.p2.x;
      yd := tri.p0.y - tri.p2.y;
      zd := tri.p0.z - tri.p2.z;

      dx := (Real(xn)*tg.xdiff + tg.xmin)-x;
      dy := (Real(yn)*tg.ydiff + tg.ymin)-y;
      dz := (Real(zn)*tg.zdiff + tg.zmin)-z;

      exy := dx*yd-dy*xd;
      exz := dx*zd-dz*xd;
      ezy := dz*yd-dy*zd;

      while (ig) loop
         OList.insert(tg.gp(xn,yn,zn).obj_list, Object_ptr(tri));
         --     Now go to next cell.
         if (exy <  0.0) then
            if (exz <  0.0) then
               if (xd <0.0) then
                  x := x-tg.xdiff;
               else
                  x :=x+tg.xdiff;
               end if;
               exy := exy + abs(yd);
               exz := exz + abs(zd);
            else
               if (zd < 0.0) then
                  z := z-tg.zdiff;
               else
                  z := z+tg.zdiff;
               end if;
               exz := exz - abs(xd);
               ezy := ezy + abs(yd);
            end if;
         else
            if (ezy < 0.0) then
               if (zd < 0.0) then
                  z := z-tg.zdiff;
               else
                  z :=z+tg.zdiff;
               end if;
               exz := exz - abs(xd);
               ezy := ezy + abs(yd);
            else
               if (yd < 0.0) then
                  y := y-tg.ydiff;
               else
                  y :=y+tg.ydiff;
               end if;
               exy := exy - abs(xd);
               ezy := ezy - abs(zd);
            end if;
         end if;


         cell_of(tg, x, y, z, xn,yn,zn,ig);
      end loop;
   end add;

   function cell_at(tg : grid_ptr; xn, yn, zn : Natural) return cell is
   begin
      return tg.gp(xn,yn,zn);
   end cell_at;

   procedure cell_of(tg : in grid_ptr; x,y,z : in Real; xn, yn, zn : out Natural; in_grid : out boolean) is
      t1, t2, t3 : Real;
   begin
      in_grid := true;

      if (x<=tg.xmin) then
         xn := 0;
         in_grid := false;
      elsif (x >= tg.xmax) then
         xn := tg.gp'Last(2);
         in_grid := false;
      else
         t1 := (x-tg.xmin)/(tg.xdiff);
         xn := Natural(Real'Floor(t1));
      end if;

      if (y<=tg.ymin) then
         yn := 0;
         in_grid := false;
      elsif (y >= tg.ymax) then
         yn := tg.gp'Last(1);
         in_grid := false;
      else
         t2 := (y-tg.ymin)/(tg.ydiff);

         yn := Natural(Real'Floor(t2));
      end if;

      if (z<=tg.zmin) then
         zn := 0;
         in_grid := false;
      elsif (z >= tg.zmax) then
         zn := tg.gp'Last(3);
         in_grid := false;
      else
         t3 := (z-tg.zmin)/(tg.zdiff);

         zn := Natural(Real'Floor(t3));
      end if;
   end cell_of;
end grid;

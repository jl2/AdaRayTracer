with OpenGL;

--  $Id: ada_sphere_procs.ads,v 1.3 2003/01/10 20:24:44 obry Exp $

package testGL_procs is
   beenDrawn : boolean;

   procedure Display;
   --pragma Convention (C, Display);

   procedure Reshape (ww : in Integer; wh : in Integer);
   --pragma Convention (C, Reshape);

   procedure Menu (value : in Integer);
   --pragma Convention (C, Menu'Access);

   procedure Init;
   --pragma Convention (C, Init);
   
   procedure Idle;
   --pragma Convention(C, Idle);

end testGL_procs;

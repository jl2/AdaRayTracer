-- ray_gl.ads

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

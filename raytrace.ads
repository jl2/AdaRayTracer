with Scene;
with Basics;

package raytrace is
	function trace(s : Scene.Scene; r : Basics.Ray) return Basics.ISect;
	procedure shade(s : in out Scene.Scene; ist : Basics.ISect; retval : out Basics.Color);
end raytrace;

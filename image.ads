with Basics;

package Image is
	type Image is array (Positive range <>, Positive Range <>) of Basics.Color;
	type Image_ptr is access all Image;

	procedure Set_Pixel(img : in out Image_ptr; x,y : in Natural; c : in Basics.Color);
	function Get_Pixel(img : Image_ptr; x, y : Natural) return Basics.Color;

	procedure Write_image(img : Image_ptr; fileName : String);
	
	function Width(img : Image_ptr) return Positive;
	function Height(img : Image_ptr) return Positive;
	
end Image;

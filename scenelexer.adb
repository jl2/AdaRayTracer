with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Text_IO;

package scenelexer is
	type lexer is record
		inf : Ada.Text_IO.File_Type;
	end record;
	--type token is (invalid, camera, sphere, poly, lbrace, rbrace, position, 
	--	direction, up, number, lbrack, rbrack, IOR, Ka, Kd, Ks, Kt, Kr, quote, comma); 
	procedure initialize(l : lexer; fn : String) is
	begin
		Open(l.inf, IN_FILE, fn);
	end initialize;
	
	procedure lex(l : lexer; t : out token; lexeme : Ada.Strings.Unbounded) is
	begin
		
end scenelexer;

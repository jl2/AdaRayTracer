with Ada.Text_IO;
with Ada.Strings.Unbounded;

package scenelexer is
	type lexer is record
		inf : Ada.Text_IO.File_Type;
	end record;
	type token is (invalid, camera, sphere, poly, lbrace, rbrace, position, direction, up, number, lbrack, rbrack, IOR, Ka, Kd, Ks, Kt, Kr, quote, comma, 
	procedure lex(l : lexer; t : out token; lexeme : Ada.Strings.Unbounded);
end scenelexer;

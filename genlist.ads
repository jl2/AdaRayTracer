--	File: genlist.ads
--	By: Jeremiah LaRocco

--	Declarations used by generic lists.

generic
	type Item is private;
	with function "="(a : Item; b : Item) return boolean;
package GenList is
	type List_b is private;
	type List is access all List_b;
	type foreach is access procedure (i : in Item);
	no_such_item : exception;
	already_there : exception;

	procedure insert(L : in out List; itm : in Item);
	procedure initialize(L : in out List);
	procedure destroy(L : in out List);
	function at_index(L : List; index : Positive) return Item;
	function index_of(L : List; itm : Item) return Positive;

	function size(L : List) return Natural;

	procedure do_for_each(L: in List; f : in foreach);

	function union(L1, l2 : in  List) return List;
	function contains(L : List; itm : Item) return Boolean;
	function is_last(L : List) return boolean;
	procedure remove(L: in out List; it : in Item);


private
	type List_b is record
		itm : Item;
		next : List;
	end record;

end GenList;

--	File: genlist.adb
--	By: Jeremiah LaRocco

--	Generic list procedure definitions

with Ada.Text_IO;
use Ada.Text_IO;
package body GenList is
	procedure insert(L : in out List; itm : in Item) is
	begin
		if (L=null) then
			L := new List_b'(itm, null);
		else
			if (l.itm = itm) then
				null;
			else
				insert(L.next, itm);
			end if;
		end if;
	end insert;
	procedure initialize(L : in out List) is
	begin
		L := null;
	end initialize;
	
	procedure destroy(L : in out List) is
	begin
		--	Should free memory here.
		L := null;
	end destroy;
	
	function at_index(L : List; index : Positive) return Item is
	begin
		if (L = null) then
			raise no_such_item;
		end if;
		if (index = 1) then
			return L.itm;
		elsif (L.next=null) and (index>1) then
			raise no_such_item;
		else
			return at_index(L.next, index-1);
		end if;
	end at_index;
	
	function index_of(L : List; itm : Item) return Positive is
	begin
		if (L=null) then
			raise no_such_item;
		elsif (l.itm=itm) then
			return 1;
		else
			return 1+index_of(L.next, itm);
		end if;
	end index_of;

	function size(L : List) return Natural is
	begin
		if (L=null) then
			return 0;
		else
			return 1+size(L.next);
		end if;
	end size;

	procedure do_for_each(L : in List; f : in foreach) is
	begin
		if (L=null) then
			null;
		else
			f(L.all.itm);
			do_for_each(L.next, f);
		end if;
	end do_for_each;

	function contains(L : List; itm : Item) return Boolean is
	begin
		if (L = null) then
			return false;
		end if;
		if (L.itm = itm) then
			return true;
		else
			return contains(L.next, itm);
		end if;
	end contains;

	function union(L1, l2 : in List) return List is
		temp : List;
		t1 : List;
		t2 : List;
	begin
		temp:=null;
		t1 := L1;

		while (t1/=null) loop
			insert(temp, t1.itm);
			t1:=t1.next;
		end loop;
		t2 := l2;

		while (t2/=null) loop
			insert(temp, t2.itm);
			t2 := t2.next;
		end loop;

		return temp;
	end union;
	function is_last(L : List) return boolean is
	begin
		return (L.next = null);
	end is_last;

	procedure remove(L: in out List; it : in Item) is
		t1 : List;
		t2 : List;
	begin
		if (L/=null) then
			if (l.itm = it) then
				if (L.next=null) then
					L:= null;
				else
					l:=L.next;
				end if;
			else
				t1 := L.next;
				t2 := L;
				search_loop:
				loop
					exit when (t1=null);
					if (t1.itm=it) then
						t2.next:=t1.next;
						t1:=null;
						exit search_loop;
					end if;
					t2 := t1;
					t1 := t1.next;
				end loop search_loop;
			end if;
		end if;
	end remove;
		
		

end GenList;

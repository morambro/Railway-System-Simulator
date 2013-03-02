with Ada.Text_IO;

package body Traveler is

	function Get_Name(Traveler : Traveler_Manager) return String is
	begin
		return Unbounded_Strings.To_String(Traveler.Traveler.Name);
	end Get_Name;

	procedure Print(T : Traveler_Manager) is
	begin
		Ada.Text_IO.Put_Line("ID : " & Integer'Image(T.Traveler.ID));
		Ada.Text_IO.Put_Line("Name : " & Unbounded_Strings.To_String(T.Traveler.Name));
		Ada.Text_IO.Put_Line("Surname : " & Unbounded_Strings.To_String(T.Traveler.Surname));
		Ada.Text_IO.Put_Line("Next Op : " & Integer'Image(T.Next_Operation));
		Ada.Text_IO.Put_Line("Destination : " & Integer'Image(T.Destination));
    end Print;

---------------------------------------- JSON-Traveler Creation ----------------------------------------------

    function Get_Traveler(Json_Traveler : JSON_Value) return Traveler_Type is
    	T : Traveler_Type;
    begin
		T.ID 		:= Json_Traveler.Get("id");
		T.Name 		:= Json_Traveler.Get("name");
		T.Surname 	:= Json_Traveler.Get("surname");
		return T;
    end;

    function Get_Traveler_Manager(Json_Traveler : JSON_Value) return Traveler_Manager is
    	T : Traveler_Manager;
    begin
    	T.Traveler		 := Get_Traveler(Json_Traveler.Get("traveler"));
		T.Next_Operation := Json_Traveler.Get("next_operation");
		T.Destination	 := Json_Traveler.Get("destination");
		return T;
    end Get_Traveler_Manager;

    function Get_Traveler_Manager_Array(Json_Traveler : JSON_Value) return Traveler_Manager_Array is
    	A_JSON_Array : constant JSON_Array := Get (Val => Json_Traveler,Field => "travelers");
	    A_JSON_Value : JSON_Value;
	    Array_Length : constant Natural := Length (A_JSON_Array);
		T : Traveler_Manager_Array(1 .. Array_Length);
    begin
    	for J in 1 .. Array_Length loop
			A_JSON_Value := Get (Arr => A_JSON_Array,Index => J);
			T(J) := Get_Traveler_Manager(A_JSON_Value);
	    end loop;
		return T;
    end Get_Traveler_Manager_Array;

    function Get_Traveler_Manager_Array(Json_Traveler : String) return Traveler_Manager_Array is
    begin
    	return Get_Traveler_Manager_Array(Get_Json_Value(Json_Traveler));
    end Get_Traveler_Manager_Array;

end Traveler;

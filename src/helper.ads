with Ada.Text_IO;
with Ada.Strings.Unbounded;

package Helper is

	package Unbounded_Strings renames Ada.Strings.Unbounded;
	use Unbounded_Strings;

	package Float_IO is new Ada.Text_IO.Float_IO(Float);


	function Get_String(Float_Number : Float;Float_Dim : Positive) return String;

end Helper;
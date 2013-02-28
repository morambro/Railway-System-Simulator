with Gnatcoll.JSON;use Gnatcoll.JSON;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package JSON_Helper is

	package SU   renames Ada.Strings.Unbounded;

	function Load_File(File_Name : String) return String;

	function GetJsonValue(Json_File_Name : String) return JSON_Value;

	procedure Handler (Name  : in UTF8_String;Value : in JSON_Value);

	procedure PrintJson(Text : String);

--  	function "<" (a,b : SU.Unbounded_String) return Boolean;
--  	function "=" (a,b : SU.Unbounded_String) return Boolean;
--
--  	package Field_Map is new Ada.Containers.Ordered_Maps
--  		(Key_Type => SU.Unbounded_String,
--  		Element_Type => SU.Unbounded_String);


end Json_Helper;
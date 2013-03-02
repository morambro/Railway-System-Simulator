package body Helper is

	function Get_String(Float_Number : Float;Float_Dim : Positive) return String is
		To_Ret : String(1 .. Float_Dim);
	begin
		Float_IO.Put(To => To_Ret,Item => Float_Number,Aft => 1,Exp => 0);
		return To_Ret;
	end;

end Helper;
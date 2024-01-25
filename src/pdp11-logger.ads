package Pdp11.Logger is

   procedure Start (Path : String);
   procedure Stop;

   procedure Put (Text : String);
   procedure Put_Line (Text : String);
   procedure New_Line;

end Pdp11.Logger;

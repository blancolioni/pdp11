with Ada.Characters.Handling;
with Ada.Strings.Fixed;

function Pdp11.Devices.Command_Line.Parse
  (Line : String)
   return Device_Command_Line'Class
is
   use Ada.Strings, Ada.Strings.Fixed;
   use Ada.Characters.Handling;
   First        : Boolean := True;
   Start        : Natural := Index_Non_Blank (Line);
   Command_Line : Device_Command_Line;
begin
   while Start > 0 loop
      pragma Assert (not Is_Space (Line (Start)));
      declare
         Next  : constant Natural := Index (Line, " ", Start + 1);
         Value : constant String :=
                   (if Next > 0
                    then Line (Start .. Next - 1)
                    else Line (Start .. Line'Last));
      begin
         pragma Assert (Value /= "");
         if First then
            First := False;
            Command_Line.Command :=
              Device_Commands.To_Bounded_String (Value);
         else
            Command_Line.Arguments.Append (Value);
         end if;
         if Next > 0 then
            Start := Index_Non_Blank (Line, Next);
         else
            Start := 0;
         end if;
      end;
   end loop;
   return Command_Line;
end Pdp11.Devices.Command_Line.Parse;

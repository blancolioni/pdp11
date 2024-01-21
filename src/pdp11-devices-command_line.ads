private with Ada.Strings.Bounded;
private with Ada.Containers.Indefinite_Vectors;

package Pdp11.Devices.Command_Line is

   type Device_Command_Line is tagged private;

   function Command (This : Device_Command_Line'Class) return String;
   function Argument_Count (This : Device_Command_Line'Class) return Natural;
   function Argument
     (This  : Device_Command_Line'Class;
      Index : Positive)
      return String
     with Pre => Index <= This.Argument_Count;

   function Argument
     (This  : Device_Command_Line'Class;
      Index : Positive)
      return Word_16
     with Pre => Index <= This.Argument_Count;

private

   package Device_Commands is
     new Ada.Strings.Bounded.Generic_Bounded_Length (32);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Device_Command_Line is tagged
      record
         Command   : Device_Commands.Bounded_String;
         Arguments : String_Vectors.Vector;
      end record;

   function Command (This : Device_Command_Line'Class) return String
   is (Device_Commands.To_String (This.Command));

   function Argument_Count (This : Device_Command_Line'Class) return Natural
   is (This.Arguments.Last_Index);

   function Argument
     (This  : Device_Command_Line'Class;
      Index : Positive)
      return String
   is (This.Arguments (Index));

   function Argument
     (This  : Device_Command_Line'Class;
      Index : Positive)
      return Word_16
   is (Word_16'Value ("8#" & This.Arguments (Index) & "#"));

end Pdp11.Devices.Command_Line;

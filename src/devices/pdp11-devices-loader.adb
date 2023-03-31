with WL.String_Maps;

with Pdp11.Devices.Command_Line.Parse;
with Pdp11.Devices.Line_Clock;
with Pdp11.Devices.RAM;
with Pdp11.Devices.RF11;
with Pdp11.Devices.ROM;
with Pdp11.Devices.TTY;

package body Pdp11.Devices.Loader is

   package Loader_Maps is
     new WL.String_Maps (Loader_Function);

   Loaders : Loader_Maps.Map;

   ----------
   -- Load --
   ----------

   function Load
     (Command : String;
      Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class)
      return Reference
   is
      Command_Line : constant Devices.Command_Line.Device_Command_Line'Class :=
                       Devices.Command_Line.Parse (Command);
   begin
      if not Loaders.Contains (Command_Line.Command) then
         raise Constraint_Error with
           "no such device: " & Command_Line.Command;
      end if;
      return Loaders.Element (Command_Line.Command) (Command_Line, Bus);
   end Load;

   --------------
   -- Register --
   --------------

   procedure Register (Name : String; Loader : Loader_Function) is
   begin
      Loaders.Insert (Name, Loader);
   end Register;

begin
   Register ("line-clock", Pdp11.Devices.Line_Clock.Load'Access);
   Register ("ram", Pdp11.Devices.RAM.Load'Access);
   Register ("rf11", Pdp11.Devices.RF11.Load'Access);
   Register ("rom", Pdp11.Devices.ROM.Load'Access);
   Register ("tty", Pdp11.Devices.TTY.Load'Access);
end Pdp11.Devices.Loader;

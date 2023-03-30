with Pdp11.Devices.Command_Line;

package Pdp11.Devices.Loader is

   type Loader_Function is access
     function (Command : Command_Line.Device_Command_Line'Class)
               return Reference;

   procedure Register
     (Name   : String;
      Loader : Loader_Function);

   function Load
     (Command : String)
      return Reference;

end Pdp11.Devices.Loader;

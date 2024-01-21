with Pdp11.Addressable;
with Pdp11.Devices.Command_Line;

package Pdp11.Devices.Loader is

   type Loader_Function is access
     function (Command : Command_Line.Device_Command_Line'Class;
               Bus     : not null access
                 Pdp11.Addressable.Root_Addressable_Type'Class)
               return Reference;

   procedure Register
     (Name   : String;
      Loader : Loader_Function);

   function Load
     (Command : String;
      Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class)
      return Reference;

end Pdp11.Devices.Loader;

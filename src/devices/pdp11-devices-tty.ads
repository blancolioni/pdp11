with Pdp11.Addressable;
with Pdp11.Devices.Command_Line;

package Pdp11.Devices.TTY is

   function Load
     (Command : Command_Line.Device_Command_Line'Class;
      Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class)
      return Reference;

end Pdp11.Devices.TTY;

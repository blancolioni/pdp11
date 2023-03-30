with Pdp11.Devices.Command_Line;

package Pdp11.Devices.TTY is

   function Load
     (Command : Command_Line.Device_Command_Line'Class)
      return Reference;

end Pdp11.Devices.TTY;

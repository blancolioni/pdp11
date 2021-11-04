with Pdp11.Drivers;

package Pdp11.Drivers.RAM is

   function Create_RAM
     (Last : Address_Type)
      return Pdp11.Drivers.Pdp11_Driver;

end Pdp11.Drivers.RAM;

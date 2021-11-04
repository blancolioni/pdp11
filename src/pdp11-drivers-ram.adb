package body Pdp11.Drivers.RAM is

   type Word_8_Array is array (Address_Type range <>) of Word_8;

   type RAM_Driver (Last : Address_Type) is
     new Pdp11.Drivers.Root_Driver_Type with
      record
         M : Word_8_Array (0 .. Last);
      end record;

   overriding function Name (Driver : RAM_Driver) return String
   is ("RAM");

   overriding function Bound (Driver : RAM_Driver) return Address_Type
   is (Driver.Last + 1);

   overriding function Get_Word_8
     (Driver : RAM_Driver;
      Address : Address_Type)
      return Word_8;

   overriding procedure Set_Word_8
     (Driver  : in out RAM_Driver;
      Address : Address_Type;
      Value   : Word_8);

   ----------------
   -- Create_RAM --
   ----------------

   function Create_RAM
     (Last : Address_Type)
      return Pdp11.Drivers.Pdp11_Driver
   is
   begin
      return new RAM_Driver'
        (Pdp11.Drivers.Root_Driver_Type with
         Last => Last,
         M    => (others => 0));
   end Create_RAM;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding function Get_Word_8
     (Driver  : RAM_Driver;
      Address : Address_Type)
      return Word_8
   is
   begin
      return Driver.M (Address);
   end Get_Word_8;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (Driver  : in out RAM_Driver;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      Driver.M (Address) := Value;
   end Set_Word_8;

end Pdp11.Drivers.RAM;

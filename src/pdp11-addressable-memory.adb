with Ada.Text_IO;

with Pdp11.Images;
with Pdp11.Options;

package body Pdp11.Addressable.Memory is

   ----------------
   -- Add_Driver --
   ----------------

   procedure Add_Driver
     (This     : not null access Root_Memory_Type'Class;
      Driver   : not null access Pdp11.Drivers.Root_Driver_Type'Class;
      Base     : Address_Type)
   is
      use type Pdp11.Drivers.Pdp11_Driver;
      Index : Driver_Index := 1;
      Bound : constant Address_Type := Base + Driver.Bound;
   begin
      while This.Installed_Drivers (Index).Driver /= null loop
         if Index = Driver_Index'Last then
            raise Constraint_Error with
              "too many drivers";
         end if;
         Index := Index + 1;
      end loop;

      if not Pdp11.Options.Quiet then
         Ada.Text_IO.Put_Line
           ("Device" & Index'Image & ": " & Driver.Name & " at "
            & Pdp11.Images.Hex_Image (Word_16 (Base))
            & "-"
            & Images.Hex_Image (Word_16 (Base + Driver.Bound - 1)));
      end if;

      This.Installed_Drivers (Index) :=
        Driver_Record'
          (Base   => Base,
           Driver => Pdp11.Drivers.Pdp11_Driver (Driver));

      for Addr in Base .. Bound - 1 loop
         This.Driver_Map (Addr) := Index;
      end loop;

      --  Driver.Install_Driver
      --    (Handler            => This,
      --     Interrupt_Priority => Priority,
      --     Interrupt_Vector   => Vector);

   end Add_Driver;

   -------------------
   -- Clear_Drivers --
   -------------------

   procedure Clear_Drivers
     (This : in out Root_Memory_Type'Class)
   is
   begin
      This.Installed_Drivers :=
        (others => (0, null));
      This.Driver_Map := (others => 0);
   end Clear_Drivers;

   ------------
   -- Create --
   ------------

   function Create
     return Memory_Reference
   is
   begin
      return new Root_Memory_Type;
   end Create;

   ------------------
   -- Get_Float_32 --
   ------------------

   overriding function Get_Float_32
     (Memory  : Root_Memory_Type;
      Address : Address_Type)
      return Float_32
   is
      use Pdp11.Drivers;
      Index  : constant Driver_Index :=
                 Memory.Driver_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Drivers (Index).Base;
      Driver : constant Pdp11_Driver :=
                 Memory.Installed_Drivers (Index).Driver;
   begin
      if Driver = null then
         raise Bad_Address;
      else
         return Driver.Get_Float_32 (Address - Base);
      end if;
   end Get_Float_32;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding function Get_Word_8
     (Memory  : Root_Memory_Type;
      Address : Address_Type)
      return Word_8
   is
      use Pdp11.Drivers;
      Index  : constant Driver_Index :=
                 Memory.Driver_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Drivers (Index).Base;
      Driver : constant Pdp11_Driver :=
                 Memory.Installed_Drivers (Index).Driver;
   begin
      if Driver = null then
         raise Bad_Address;
      else
         return Driver.Get_Word_8 (Address - Base);
      end if;
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   overriding function Get_Word_16
     (Memory  : Root_Memory_Type;
      Address : Address_Type)
      return Word_16
   is
      use Pdp11.Drivers;
      Index  : constant Driver_Index :=
                 Memory.Driver_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Drivers (Index).Base;
      Driver : constant Pdp11_Driver :=
                 Memory.Installed_Drivers (Index).Driver;
   begin
      if Driver = null then
         raise Bad_Address;
      else
         return Driver.Get_Word_16 (Address - Base);
      end if;
   end Get_Word_16;

   ------------------
   -- Set_Float_32 --
   ------------------

   overriding procedure Set_Float_32
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Float_32)
   is
      use Pdp11.Drivers;
      Index  : constant Driver_Index :=
                 Memory.Driver_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Drivers (Index).Base;
      Driver : constant Pdp11_Driver :=
                 Memory.Installed_Drivers (Index).Driver;
   begin
      if Driver /= null then
         Driver.Set_Float_32 (Address - Base, Value);
      else
         raise Bad_Address;
      end if;
   end Set_Float_32;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_8)
   is
      use Pdp11.Drivers;
      Index  : constant Driver_Index :=
                 Memory.Driver_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Drivers (Index).Base;
      Driver : constant Pdp11_Driver :=
                 Memory.Installed_Drivers (Index).Driver;
   begin
      if Driver /= null then
         Driver.Set_Word_8 (Address - Base, Value);
      else
         raise Bad_Address;
      end if;
   end Set_Word_8;

   -----------------
   -- Set_Word_16 --
   -----------------

   overriding procedure Set_Word_16
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Word_16)
   is
      use Pdp11.Drivers;
      Index  : constant Driver_Index :=
                 Memory.Driver_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Drivers (Index).Base;
      Driver : constant Pdp11_Driver :=
                 Memory.Installed_Drivers (Index).Driver;
   begin
      if Driver /= null then
         Driver.Set_Word_16 (Address - Base, Value);
      else
         raise Bad_Address with "bad address: "
           & Pdp11.Images.Hex_Image (Word_16 (Address));
      end if;
   end Set_Word_16;

end Pdp11.Addressable.Memory;

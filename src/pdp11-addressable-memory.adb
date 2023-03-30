with Ada.Text_IO;

with Pdp11.Images;
with Pdp11.Options;

package body Pdp11.Addressable.Memory is

   procedure Raise_Bad_Address
     (Address : Address_Type);

   ----------------
   -- Add_Device --
   ----------------

   procedure Add_Device
     (This     : not null access Root_Memory_Type'Class;
      Device   : not null access Pdp11.Devices.Instance'Class)
   is
      use type Pdp11.Devices.Reference;
      Index : Device_Index := 1;
   begin
      while This.Installed_Devices (Index).Device /= null loop
         if Index = Device_Index'Last then
            raise Constraint_Error with
              "too many devices";
         end if;
         Index := Index + 1;
      end loop;

      if not Pdp11.Options.Quiet then
         Ada.Text_IO.Put_Line
           ("Device" & Index'Image & ": " & Device.Name & " at "
            & Pdp11.Images.Hex_Image (Word_16 (Device.Base))
            & "-"
            & Images.Hex_Image (Word_16 (Device.Bound)));
      end if;

      This.Installed_Devices (Index) :=
        Device_Record'
          (Base   => Device.Base,
           Device => Pdp11.Devices.Reference (Device));

      for Addr in Device.Base .. Device.Bound loop
         This.Device_Map (Addr) := Index;
      end loop;

      --  Device.Install_Device
      --    (Handler            => This,
      --     Interrupt_Priority => Priority,
      --     Interrupt_Vector   => Vector);

   end Add_Device;

   -------------------
   -- Clear_Devices --
   -------------------

   procedure Clear_Devices
     (This : in out Root_Memory_Type'Class)
   is
   begin
      This.Installed_Devices :=
        (others => (0, null));
      This.Device_Map := (others => 0);
   end Clear_Devices;

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

   overriding procedure Get_Float_32
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : out Float_32)
   is
      use Pdp11.Devices;
      Index  : constant Device_Index :=
                 Memory.Device_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Devices (Index).Base;
      Device : constant Reference :=
                 Memory.Installed_Devices (Index).Device;
   begin
      if Device = null then
         Raise_Bad_Address (Address);
      else
         Device.Get_Float_32 (Address - Base, Value);
      end if;
   end Get_Float_32;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : out Word_8)
   is
      use Pdp11.Devices;
      Index  : constant Device_Index :=
                 Memory.Device_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Devices (Index).Base;
      Device : constant Reference :=
                 Memory.Installed_Devices (Index).Device;
   begin
      if Device = null then
         Raise_Bad_Address (Address);
      else
         Device.Get_Word_8 (Address - Base, Value);
      end if;
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   overriding procedure Get_Word_16
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : out Word_16)
   is
      use Pdp11.Devices;
      Index  : constant Device_Index :=
                 Memory.Device_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Devices (Index).Base;
      Device : constant Reference :=
                 Memory.Installed_Devices (Index).Device;
   begin
      if Device = null then
         Raise_Bad_Address (Address);
      else
         Device.Get_Word_16 (Address - Base, Value);
      end if;
   end Get_Word_16;

   -----------------------
   -- Raise_Bad_Address --
   -----------------------

   procedure Raise_Bad_Address
     (Address : Address_Type)
   is
   begin
      raise Bad_Address with
        "invalid address: " & Pdp11.Images.Octal_Image (Word_16 (Address));
   end Raise_Bad_Address;

   ------------------
   -- Set_Float_32 --
   ------------------

   overriding procedure Set_Float_32
     (Memory  : in out Root_Memory_Type;
      Address : Address_Type;
      Value   : Float_32)
   is
      use Pdp11.Devices;
      Index  : constant Device_Index :=
                 Memory.Device_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Devices (Index).Base;
      Device : constant Reference :=
                 Memory.Installed_Devices (Index).Device;
   begin
      if Device /= null then
         Device.Set_Float_32 (Address - Base, Value);
      else
         Raise_Bad_Address (Address);
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
      use Pdp11.Devices;
      Index  : constant Device_Index :=
                 Memory.Device_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Devices (Index).Base;
      Device : constant Reference :=
                 Memory.Installed_Devices (Index).Device;
   begin
      if Device /= null then
         Device.Set_Word_8 (Address - Base, Value);
      else
         Raise_Bad_Address (Address);
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
      use Pdp11.Devices;
      Index  : constant Device_Index :=
                 Memory.Device_Map (Address);
      Base   : constant Address_Type :=
                 Memory.Installed_Devices (Index).Base;
      Device : constant Reference :=
                 Memory.Installed_Devices (Index).Device;
   begin
      if Device /= null then
         Device.Set_Word_16 (Address - Base, Value);
      else
         Raise_Bad_Address (Address);
      end if;
   end Set_Word_16;

end Pdp11.Addressable.Memory;

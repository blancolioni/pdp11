with System.Storage_Elements;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Pdp11.Images;

package body Pdp11.Devices.RF11 is

   Base_Address       : constant := 8#177460#;
   Bound_Address      : constant := 8#177477#;

   DCS : constant := 0;
   WC  : constant := 1;
   CMA : constant := 2;
   DAR : constant := 3;
   DAE : constant := 4;
   --  DDB : constant := 5;
   --  MA  : constant := 6;
   --  ADS : constant := 7;

   GO  : constant := 0;

   subtype Parent is Pdp11.Devices.Instance;

   subtype Register_Address is Address_Type range 8#000# .. 8#007#;

   type Register_Array is array (Register_Address) of Word_16;

   Read_Mask : constant Register_Array :=
                 (0      => 2#1111_1110_1111_1110#,
                  others => 8#177_777#);

   Write_Mask : constant Register_Array :=
                  (0      => 2#0000_0001_0111_1111#,
                   others => 8#177_777#);

   type Disk_State is
     (Idle, Seeking, Transferring);

   type Transfer_Type is (No_Operation, Write, Read, Write_Check);

   Disk_Unit_Count  : constant := 8;
   Track_Count      : constant := 128;
   Track_Length     : constant := 2048;
   Disk_Words       : constant := Track_Length * Track_Count;
   Disk_Bytes       : constant := Disk_Words * 2;
   Total_Disk_Store : constant := Disk_Unit_Count * Disk_Bytes;

   subtype Disk_Storage_Array is
     System.Storage_Elements.Storage_Array (1 .. Total_Disk_Store);

   package Disk_Storage_IO is
     new Ada.Sequential_IO (Disk_Storage_Array);

   subtype Disk_Path is String (1 .. 64);

   type Instance is new Parent with
      record
         Rs        : Register_Array := (others => 0);
         State     : Disk_State := Idle;
         Transfer  : Transfer_Type := No_Operation;
         Wait_Time : Pdp11.ISA.Microsecond_Duration := 0.0;
         Bus       : Pdp11.Addressable.Addressable_Reference;
         Image     : Disk_Storage_Array;
         Path      : Disk_Path;
      end record;

   type Reference is access all Instance'Class;

   overriding function Name (This : Instance) return String
   is ("rf11");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class);

   overriding procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16);

   overriding procedure Set_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_16);

   function DCS_Bits (This               : Instance'Class;
                      Lo_Bit, Hi_Bit : Natural)
                      return Word_16
   is (This.Rs (DCS) / 2 ** Lo_Bit mod 2 ** (Hi_Bit - Lo_Bit + 1));

   function DCS_Bit (This : Instance'Class;
                     Bit  : Natural)
                     return Boolean
   is (This.Rs (DCS) / 2 ** Bit mod 2 = 1);

   procedure DCS_Clear
     (This : in out Instance'Class;
      Bit  : Natural);

   procedure DCS_Set
     (This : in out Instance'Class;
      Bit  : Natural);

   ---------------
   -- DCS_Clear --
   ---------------

   procedure DCS_Clear
     (This : in out Instance'Class;
      Bit  : Natural)
   is
      X : Word_16 renames This.Rs (DCS);
   begin
      X := X and not (2 ** Bit);
   end DCS_Clear;

   -------------
   -- DCS_Set --
   -------------

   procedure DCS_Set
     (This : in out Instance'Class;
      Bit  : Natural)
   is
      X : Word_16 renames This.Rs (DCS);
   begin
      X := X or 2 ** Bit;
   end DCS_Set;

   -----------------
   -- Get_Word_16 --
   -----------------

   overriding procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16)
   is
   begin
      Value := This.Rs (Address / 2) and Read_Mask (Address / 2);
   end Get_Word_16;

   ----------
   -- Load --
   ----------

   function Load
     (Command : Command_Line.Device_Command_Line'Class;
      Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class)
      return Pdp11.Devices.Reference
   is
      use Disk_Storage_IO;
      File : File_Type;
      Path : constant String := Command.Argument (1);
      This : constant Reference := new Instance'
        (Pdp11.Devices.Parent with
         Priority => 5,
         Vector   => 8#210#,
         Base     => Base_Address,
         Bound    => Bound_Address,
         Bus      => Pdp11.Addressable.Addressable_Reference (Bus),
         Path     => (others => ' '),
         others   => <>);
   begin
      This.Path (1 .. Path'Length) := Path;
      Open (File, In_File, Path);
      Read (File, This.Image);
      Close (File);
      return Pdp11.Devices.Reference (This);
   end Load;

   -----------------
   -- Set_Word_16 --
   -----------------

   overriding procedure Set_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_16)
   is
   begin
      This.Rs (Address / 2) :=
        (This.Rs (Address / 2) and not Write_Mask (Address / 2))
        or (Value and Write_Mask (Address / 2));
   end Set_Word_16;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is
      use type Pdp11.ISA.Microsecond_Duration;
   begin
      if Elapsed < This.Wait_Time then
         This.Wait_Time := This.Wait_Time - Elapsed;
         return;
      end if;

      case This.State is
         when Idle =>
            if This.DCS_Bit (GO) then
               This.DCS_Clear (GO);
               This.DCS_Clear (7);
               This.State := Seeking;
               This.Wait_Time := 1000.0;
            end if;

         when Seeking =>
            This.Transfer := Transfer_Type'Val (This.DCS_Bits (1, 2));
            This.State := Transferring;
            This.Wait_Time := 20_000.0;

         when Transferring =>
            declare
               use System.Storage_Elements;
               A : constant Storage_Offset :=
                     (Storage_Offset (This.Rs (DAR))
                      + Storage_Offset (This.Rs (DAE) mod 32) * 65536)
                     * 2
                       + 1;
               W : constant Word_16 :=
                     Word_16 (This.Image (A))
                     + 256 * Word_16 (This.Image (A + 1));
            begin
               if False then
                  Ada.Text_IO.Put_Line
                    (Pdp11.Images.Hex_Image (Word_8 (A / 65536))
                     & ":"
                     & Pdp11.Images.Hex_Image (Word_16 (A mod 65536))
                     & " -> "
                     & Pdp11.Images.Hex_Image (This.Rs (CMA))
                     & ": "
                     & Pdp11.Images.Hex_Image (W));
               end if;
               This.Bus.Set_Word_16
                 (Address_Type (This.Rs (CMA)), W);
               This.Rs (CMA) := This.Rs (CMA) + 2;
               This.Rs (DAR) := This.Rs (DAR) + 1;
               if This.Rs (DAR)  = 0 then
                  This.Rs (DAE) :=
                    (This.Rs (DAE) and 16#FFC0#)
                      + (This.Rs (DAE) mod 32 + 1);
               end if;
               This.Rs (WC) := This.Rs (WC) + 1;
               if This.Rs (WC) = 0 then
                  This.DCS_Set (7);
                  This.State := Idle;
                  This.Transfer := No_Operation;
                  This.Wait_Time := 0.0;
               else
                  This.Wait_Time := 19.2;
               end if;
            end;
      end case;
   end Tick;

end Pdp11.Devices.RF11;

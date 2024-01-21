with Ada.Directories;

with WL.Binary_IO;

with Pdp11.Paths;

package body Pdp11.Devices.ROM is

   subtype Parent is Pdp11.Devices.Instance;

   type Word_8_Array is array (Address_Type range <>) of Word_8;

   type Instance (Last : Address_Type) is new Parent with
      record
         M : Word_8_Array (0 .. Last);

      end record;

   overriding function Name (This : Instance) return String
   is ("ROM");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is null;

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is null;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
   begin
      Value := This.M (Address);
   end Get_Word_8;

   ----------
   -- Load --
   ----------

   function Load
     (Command : Command_Line.Device_Command_Line'Class;
      Bus     : not null access
        Pdp11.Addressable.Root_Addressable_Type'Class)
      return Reference
   is
      pragma Unreferenced (Bus);
      use WL.Binary_IO;
      File : File_Type;
      Base        : constant Word_16 := Command.Argument (1);
      Local_Path  : constant String := Command.Argument (2);
      Config_Path : constant String := Pdp11.Paths.Config_File (Local_Path);
      Path        : constant String :=
                      (if Ada.Directories.Exists (Local_Path)
                       then Local_Path
                       elsif Ada.Directories.Exists (Config_Path)
                       then Config_Path
                       else (raise Constraint_Error with
                         "cannot find ROM file: " & Local_Path));
   begin

      Open (File, In_File, Path);

      declare
         Memory : Word_8_Array (0 .. Address_Type (Length (File)) - 1);
      begin
         for B of Memory loop
            declare
               W : WL.Binary_IO.Word_8;
            begin
               Read (File, W);
               B := Pdp11.Word_8 (W);
            end;
         end loop;

         Close (File);

         return new Instance'
           (Pdp11.Devices.Parent with
            Priority => <>,
            Vector   => <>,
            Base     => Address_Type (Base) + Memory'First,
            Bound    => Address_Type (Base) + Memory'Last,
            Last     => Memory'Last,
            M        => Memory);
      end;

   end Load;

end Pdp11.Devices.ROM;

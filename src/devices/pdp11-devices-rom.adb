with WL.Binary_IO;

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

   overriding function Get_Word_8
     (This    : Instance;
      Address : Address_Type)
      return Word_8
   is (This.M (Address));

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is null;

   function Create
     (Base : Address_Type;
      Path : String)
      return Reference
   is
      use WL.Binary_IO;
      File : File_Type;
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
            Base     => Base + Memory'First,
            Bound    => Base + Memory'Last,
            Last     => Memory'Last,
            M        => Memory);
      end;

   end Create;

end Pdp11.Devices.ROM;

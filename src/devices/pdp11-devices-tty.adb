with Ada.Text_IO;

package body Pdp11.Devices.TTY is

   subtype Parent is Pdp11.Devices.Instance;

   type Instance is new Parent with
      record
         Ch : Character := ' ';
      end record;

   overriding function Name (This : Instance) return String
   is ("TTY");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is null;

   overriding function Get_Word_8
     (This    : Instance;
      Address : Address_Type)
      return Word_8
   is (if Address = 0 then 0
       elsif Address = 1 then Character'Pos (This.Ch)
       else 255);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

   ------------
   -- Create --
   ------------

   function Create
      return Reference
   is
   begin
      return new Instance'
        (Pdp11.Devices.Parent with
           Priority => 4,
           Vector   => 8#040#,
           Base     => 16#FF80#,
           Bound    => 16#FF81#,
           Ch       => <>);
   end Create;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      if Address = 0 then
         if Value /= 0 then
            Ada.Text_IO.Put (This.Ch);
            Ada.Text_IO.Flush;
         end if;
      elsif Address = 1 then
         This.Ch := Character'Val (Value);
      end if;
   end Set_Word_8;

end Pdp11.Devices.TTY;

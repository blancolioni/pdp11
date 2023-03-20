with Ada.Text_IO;

package body Pdp11.Drivers.TTY is

   type TTY_Driver_Record is
     new Root_Driver_Type with
      record
         Ch : Character := ' ';
      end record;

   overriding function Bound
     (Driver : TTY_Driver_Record)
      return Address_Type
   is (2);

   overriding function Name
     (Driver : TTY_Driver_Record)
      return String
   is ("tty");

   overriding function Get_Word_8
     (Driver  : TTY_Driver_Record;
      Address : Address_Type)
      return Word_8
   is (if Address = 0 then 0
       elsif Address = 1 then Character'Pos (Driver.Ch)
       else 255);

   overriding procedure Set_Word_8
     (Driver  : in out TTY_Driver_Record;
      Address : Address_Type;
      Value   : Word_8);

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (Driver  : in out TTY_Driver_Record;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      if Address = 0 then
         if Value /= 0 then
            Ada.Text_IO.Put (Driver.Ch);
            Ada.Text_IO.Flush;
         end if;
      elsif Address = 1 then
         Driver.Ch := Character'Val (Value);
      end if;
   end Set_Word_8;

   ----------------
   -- TTY_Driver --
   ----------------

   function TTY_Driver return Pdp11.Drivers.Pdp11_Driver is
   begin
      return new TTY_Driver_Record;
   end TTY_Driver;

end Pdp11.Drivers.TTY;

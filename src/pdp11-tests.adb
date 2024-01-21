with Ada.Text_IO;

with Pdp11.Images;
with Pdp11.ISA;

package body Pdp11.Tests is

   -------------------
   -- Test_Decoding --
   -------------------

   procedure Test_Decoding is
      Available : Boolean := False;
      Start_Available : Word_16 := 0;
   begin
      for IR in Word_16 loop
         declare
            Rec  : constant ISA.Instruction_Record :=
              ISA.Decode (IR);
         begin
            if Rec.Undefined then
               if not Available then
                  Available := True;
                  Start_Available := IR;
               end if;
            else
               if Available then
                  Available := False;
                  Ada.Text_IO.Put_Line
                    ("available: "
                     & Images.Octal_Image (Start_Available)
                     & " - "
                     & Images.Octal_Image (IR - 1));
               end if;
            end if;

            if not Rec.Undefined then
               declare
                  IR_2 : constant Word_16 :=
                    ISA.Encode (Rec);
               begin
                  if IR /= IR_2
                    and then IR_2 /= 8#000240#
                  then
                     Ada.Text_IO.Put_Line
                       ("warning: "
                        & Images.Octal_Image (IR)
                        & " -> "
                        & Images.Image (Rec)
                        & " -> "
                        & Images.Octal_Image (IR_2)
                        & " -> "
                        & Images.Image (ISA.Decode (IR_2)));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Test_Decoding;

   -------------------
   -- Test_Encoding --
   -------------------

   procedure Test_Encoding is
      use ISA;
   begin
      for Instruction in Instruction_Type loop
         begin
            declare
               Rec  : constant ISA.Instruction_Record :=
                 Instruction_Record'
                   (Instruction => Instruction, others => <>);
               IR   : constant Word_16 :=
                 ISA.Encode (Rec);
               Rec_2 : constant ISA.Instruction_Record :=
                 ISA.Decode (IR);
               IR_2  : constant Word_16 :=
                 ISA.Encode (Rec_2);
            begin
               if (Rec /= Rec_2)
                 and then IR_2 /= 8#000240#
               then
                  Ada.Text_IO.Put_Line
                    ("warning: "
                     & Images.Image (Rec)
                     & " -> "
                     & Images.Octal_Image (IR)
                     & " -> "
                     & Images.Image (Rec_2)
                     & " -> "
                     & Images.Octal_Image (IR_2)
                     & " -> "
                     & Images.Image (ISA.Decode (IR_2)));
               end if;
            end;
         exception
            when others =>
               Ada.Text_IO.Put_Line
                 ("failed to decode: " & Instruction'Image);
         end;
      end loop;
   end Test_Encoding;

end Pdp11.Tests;

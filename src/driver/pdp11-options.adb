with WL.Command_Line;

package body Pdp11.Options is

   pragma Style_Checks (Off);

   function Start return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("start", ' ', 0);
   end Start;

   function Execute return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("execute", ' ');
   end Execute;

   function Object return String is
   begin
      return WL.Command_Line.Find_Option
               ("object", ' ');
   end Object;

   function Quiet return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("quiet", ' ');
   end Quiet;

   function Source return String is
   begin
      return WL.Command_Line.Find_Option
               ("source", ' ');
   end Source;

   function Test_Encoding return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("test-encoding", ' ');
   end Test_Encoding;

   function Time_Limit return Natural is
   begin
      return WL.Command_Line.Find_Option
               ("time-limit", ' ', 0);
   end Time_Limit;

   function Limit_Speed return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("limit-speed", ' ');
   end Limit_Speed;

   function Trace return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace", ' ');
   end Trace;

   function Warnings return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("warnings", ' ');
   end Warnings;

   function Create_Disk return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("create-disk", ' ');
   end Create_Disk;

   function Write_Disk return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("write-disk", ' ');
   end Write_Disk;

   function Disk_Type return String is
   begin
      return WL.Command_Line.Find_Option
               ("disk-type", ' ');
   end Disk_Type;

   function Disk_File return String is
   begin
      return WL.Command_Line.Find_Option
               ("disk-file", ' ');
   end Disk_File;

   function Disk_Address return String is
   begin
      return WL.Command_Line.Find_Option
               ("disk-address", ' ');
   end Disk_Address;

end Pdp11.Options;

pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~yami4names.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~yami4names.adb");

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "ada__containers_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ada__io_exceptions_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "ada__strings_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "ada__strings__maps_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "ada__strings__maps__constants_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__tags_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__streams_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "interfaces__c_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "interfaces__c__strings_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__exceptions_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__finalization_root_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "ada__finalization_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "system__storage_pools_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__finalization_masters_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__storage_pools__subpools_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "system__task_info_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "ada__calendar__delays_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__calendar__time_zones_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "system__assertions_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "system__pool_global_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "system__file_control_block_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "ada__streams__stream_io_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "system__file_io_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "system__secondary_stack_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "ada__strings__unbounded_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "ada__directories_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "system__os_lib_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "system__regexp_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "system__strings__stream_ops_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__tasking__initialization_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__tasking__protected_objects_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__real_time_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__text_io_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "system__tasking__protected_objects__entries_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "system__tasking__queuing_E");
   E190 : Short_Integer; pragma Import (Ada, E190, "system__tasking__stages_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "yami_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "ial__log_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "ial__properties_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "ial__tasking__starter_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "log_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "log_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "name_server__cache_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "name_server__configuration_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "yami__details_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "yami__core__closed_connection_handlers_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "yami__core__event_notification_handlers_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "yami__core__incoming_message_handlers_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "yami__core__message_progress_handlers_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "yami__core__new_connection_handlers_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "yami__serializables_E");
   E262 : Short_Integer; pragma Import (Ada, E262, "yami__parameters_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "yami__raw_buffer_data_sources_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "yami__core__agents_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "name_server__messaging_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "name_server__messaging__finalize_body");
      begin
         E228 := E228 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "yami__core__agents__finalize_body");
      begin
         E255 := E255 - 1;
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "yami__core__agents__finalize_spec");
      begin
         F3;
      end;
      E266 := E266 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "yami__raw_buffer_data_sources__finalize_spec");
      begin
         F4;
      end;
      E262 := E262 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "yami__parameters__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "yami__core__new_connection_handlers__finalize_spec");
      begin
         E260 := E260 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "yami__core__message_progress_handlers__finalize_spec");
      begin
         E259 := E259 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "yami__core__incoming_message_handlers__finalize_spec");
      begin
         E258 := E258 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "yami__core__event_notification_handlers__finalize_spec");
      begin
         E257 := E257 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "yami__core__closed_connection_handlers__finalize_spec");
      begin
         E256 := E256 - 1;
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "name_server__cache__finalize_body");
      begin
         E233 := E233 - 1;
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "ial__tasking__starter__finalize_body");
      begin
         E231 := E231 - 1;
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ial__properties__finalize_body");
      begin
         E196 := E196 - 1;
         F13;
      end;
      E172 := E172 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F14;
      end;
      E065 := E065 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "ada__text_io__finalize_spec");
      begin
         F15;
      end;
      E237 := E237 - 1;
      E249 := E249 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__regexp__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__file_io__finalize_body");
      begin
         E071 := E071 - 1;
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada__directories__finalize_spec");
      begin
         F18;
      end;
      E241 := E241 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "ada__strings__unbounded__finalize_spec");
      begin
         F19;
      end;
      E086 := E086 - 1;
      E100 := E100 - 1;
      E222 := E222 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "ada__streams__stream_io__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__file_control_block__finalize_spec");
      begin
         E084 := E084 - 1;
         F21;
      end;
      E096 := E096 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "system__pool_global__finalize_spec");
      begin
         F22;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "system__storage_pools__subpools__finalize_spec");
      begin
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "system__finalization_masters__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, True, False, False, False, False, 
           False, False, False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, True, True, False, False, False, True, 
           False, True, True, True, True, False, False, True, 
           False, False, True, True, False, True, True, True, 
           True, True, True, False, True, True, False, True, 
           False, False, True, False, True, True, False, False, 
           False, True, False, False, True, False, True, False, 
           False, False, False, True, False, True, True, True, 
           False, False, True, False, False, True, False, True, 
           True, False, True, True, True, False, True, True, 
           False, False, True, False, True, False),
         Count => (1, 0, 0, 2, 0, 3, 0),
         Unknown => (False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      Ada.Containers'Elab_Spec;
      E205 := E205 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E076 := E076 + 1;
      Ada.Strings'Elab_Spec;
      E201 := E201 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E204 := E204 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E066 := E066 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E029 := E029 + 1;
      System.Finalization_Root'Elab_Spec;
      E075 := E075 + 1;
      Ada.Finalization'Elab_Spec;
      E073 := E073 + 1;
      System.Storage_Pools'Elab_Spec;
      E094 := E094 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E140 := E140 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E006 := E006 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E052 := E052 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E106 := E106 + 1;
      System.Assertions'Elab_Spec;
      E216 := E216 + 1;
      System.Pool_Global'Elab_Spec;
      E096 := E096 + 1;
      System.File_Control_Block'Elab_Spec;
      E084 := E084 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E222 := E222 + 1;
      E100 := E100 + 1;
      System.Finalization_Masters'Elab_Body;
      E086 := E086 + 1;
      E078 := E078 + 1;
      E048 := E048 + 1;
      Ada.Tags'Elab_Body;
      E056 := E056 + 1;
      E203 := E203 + 1;
      System.Soft_Links'Elab_Body;
      E013 := E013 + 1;
      System.Secondary_Stack'Elab_Body;
      E017 := E017 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E241 := E241 + 1;
      Ada.Directories'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E081 := E081 + 1;
      System.File_Io'Elab_Body;
      E071 := E071 + 1;
      System.Regexp'Elab_Spec;
      E249 := E249 + 1;
      Ada.Directories'Elab_Body;
      E237 := E237 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E220 := E220 + 1;
      System.Tasking.Initialization'Elab_Body;
      E176 := E176 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E166 := E166 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E119 := E119 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E065 := E065 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E172 := E172 + 1;
      System.Tasking.Queuing'Elab_Body;
      E182 := E182 + 1;
      System.Tasking.Stages'Elab_Body;
      E190 := E190 + 1;
      YAMI'ELAB_SPEC;
      E250 := E250 + 1;
      E163 := E163 + 1;
      IAL.PROPERTIES'ELAB_BODY;
      E196 := E196 + 1;
      IAL.TASKING.STARTER'ELAB_BODY;
      E231 := E231 + 1;
      Log'Elab_Body;
      E102 := E102 + 1;
      Name_Server.Cache'Elab_Body;
      E233 := E233 + 1;
      E192 := E192 + 1;
      E253 := E253 + 1;
      YAMI.CORE.CLOSED_CONNECTION_HANDLERS'ELAB_SPEC;
      E256 := E256 + 1;
      YAMI.CORE.EVENT_NOTIFICATION_HANDLERS'ELAB_SPEC;
      E257 := E257 + 1;
      YAMI.CORE.INCOMING_MESSAGE_HANDLERS'ELAB_SPEC;
      E258 := E258 + 1;
      YAMI.CORE.MESSAGE_PROGRESS_HANDLERS'ELAB_SPEC;
      E259 := E259 + 1;
      YAMI.CORE.NEW_CONNECTION_HANDLERS'ELAB_SPEC;
      E260 := E260 + 1;
      YAMI.SERIALIZABLES'ELAB_SPEC;
      E264 := E264 + 1;
      YAMI.PARAMETERS'ELAB_SPEC;
      YAMI.PARAMETERS'ELAB_BODY;
      E262 := E262 + 1;
      YAMI.RAW_BUFFER_DATA_SOURCES'ELAB_SPEC;
      YAMI.RAW_BUFFER_DATA_SOURCES'ELAB_BODY;
      E266 := E266 + 1;
      YAMI.CORE.AGENTS'ELAB_SPEC;
      YAMI.CORE.AGENTS'ELAB_BODY;
      E255 := E255 + 1;
      Name_Server.Messaging'Elab_Body;
      E228 := E228 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_yami4names");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/ial.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/ial-tasking.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/name_server.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/ial-log.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/ial-properties.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/ial-tasking-starter.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/log.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/name_server-cache.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/name_server-configuration.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/name_server-storage.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/name_server-messaging.o
   --   /home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/yami4names.o
   --   -L/home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/src/services/names/
   --   -L/home/moreno/Scrivania/TrainProject/lib/yami4-gpl-1.6.0/lib/ravenscar/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.5.4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lpthread
--  END Object file/option list   

end ada_main;

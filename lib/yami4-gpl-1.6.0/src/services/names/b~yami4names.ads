pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2012 (20120509)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_yami4names" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#030e17a5#;
   pragma Export (C, u00001, "yami4namesB");
   u00002 : constant Version_32 := 16#3935bd10#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#e50e0229#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#8ba0787e#;
   pragma Export (C, u00005, "ada__calendarB");
   u00006 : constant Version_32 := 16#e791e294#;
   pragma Export (C, u00006, "ada__calendarS");
   u00007 : constant Version_32 := 16#ebcaf1b3#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#2bc1a577#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#16173147#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#e3a511ca#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#eb6e42ba#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#0071025c#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#7ad2d2f3#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#27940d94#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#5d8c4e7a#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#17775d6d#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#ff006514#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#11a33f22#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#48ccfe96#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#7b9f0bae#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#7a009e1f#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#84debe5c#;
   pragma Export (C, u00024, "system__htableB");
   u00025 : constant Version_32 := 16#68c60cb4#;
   pragma Export (C, u00025, "system__htableS");
   u00026 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00026, "system__string_hashB");
   u00027 : constant Version_32 := 16#cdf29a2e#;
   pragma Export (C, u00027, "system__string_hashS");
   u00028 : constant Version_32 := 16#aad75561#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#e7908a0d#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#010db1dc#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#d31e676e#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#e9b5a278#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#8ae996cf#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00036, "system__wch_conB");
   u00037 : constant Version_32 := 16#54856c87#;
   pragma Export (C, u00037, "system__wch_conS");
   u00038 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00038, "system__wch_stwB");
   u00039 : constant Version_32 := 16#79944086#;
   pragma Export (C, u00039, "system__wch_stwS");
   u00040 : constant Version_32 := 16#b8a9e30d#;
   pragma Export (C, u00040, "system__wch_cnvB");
   u00041 : constant Version_32 := 16#4a7bea51#;
   pragma Export (C, u00041, "system__wch_cnvS");
   u00042 : constant Version_32 := 16#129923ea#;
   pragma Export (C, u00042, "interfacesS");
   u00043 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00043, "system__wch_jisB");
   u00044 : constant Version_32 := 16#1e097145#;
   pragma Export (C, u00044, "system__wch_jisS");
   u00045 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00045, "system__traceback_entriesB");
   u00046 : constant Version_32 := 16#b94facfb#;
   pragma Export (C, u00046, "system__traceback_entriesS");
   u00047 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00047, "interfaces__cB");
   u00048 : constant Version_32 := 16#f05a3eb1#;
   pragma Export (C, u00048, "interfaces__cS");
   u00049 : constant Version_32 := 16#22d03640#;
   pragma Export (C, u00049, "system__os_primitivesB");
   u00050 : constant Version_32 := 16#5bbfce93#;
   pragma Export (C, u00050, "system__os_primitivesS");
   u00051 : constant Version_32 := 16#45724809#;
   pragma Export (C, u00051, "ada__calendar__delaysB");
   u00052 : constant Version_32 := 16#474dd4b1#;
   pragma Export (C, u00052, "ada__calendar__delaysS");
   u00053 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00053, "system__tracesB");
   u00054 : constant Version_32 := 16#19732a10#;
   pragma Export (C, u00054, "system__tracesS");
   u00055 : constant Version_32 := 16#5331c1d4#;
   pragma Export (C, u00055, "ada__tagsB");
   u00056 : constant Version_32 := 16#425ab8ea#;
   pragma Export (C, u00056, "ada__tagsS");
   u00057 : constant Version_32 := 16#818f1ecc#;
   pragma Export (C, u00057, "system__unsigned_typesS");
   u00058 : constant Version_32 := 16#68f8d5f8#;
   pragma Export (C, u00058, "system__val_lluB");
   u00059 : constant Version_32 := 16#fb7d49be#;
   pragma Export (C, u00059, "system__val_lluS");
   u00060 : constant Version_32 := 16#46a1f7a9#;
   pragma Export (C, u00060, "system__val_utilB");
   u00061 : constant Version_32 := 16#e0c3d7a5#;
   pragma Export (C, u00061, "system__val_utilS");
   u00062 : constant Version_32 := 16#b7fa72e7#;
   pragma Export (C, u00062, "system__case_utilB");
   u00063 : constant Version_32 := 16#46722232#;
   pragma Export (C, u00063, "system__case_utilS");
   u00064 : constant Version_32 := 16#bc0fac87#;
   pragma Export (C, u00064, "ada__text_ioB");
   u00065 : constant Version_32 := 16#b01682d7#;
   pragma Export (C, u00065, "ada__text_ioS");
   u00066 : constant Version_32 := 16#1358602f#;
   pragma Export (C, u00066, "ada__streamsS");
   u00067 : constant Version_32 := 16#7a48d8b1#;
   pragma Export (C, u00067, "interfaces__c_streamsB");
   u00068 : constant Version_32 := 16#a539be81#;
   pragma Export (C, u00068, "interfaces__c_streamsS");
   u00069 : constant Version_32 := 16#f1fbff23#;
   pragma Export (C, u00069, "system__crtlS");
   u00070 : constant Version_32 := 16#4a803ccf#;
   pragma Export (C, u00070, "system__file_ioB");
   u00071 : constant Version_32 := 16#e6194557#;
   pragma Export (C, u00071, "system__file_ioS");
   u00072 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00072, "ada__finalizationB");
   u00073 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00073, "ada__finalizationS");
   u00074 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00074, "system__finalization_rootB");
   u00075 : constant Version_32 := 16#a49c312a#;
   pragma Export (C, u00075, "system__finalization_rootS");
   u00076 : constant Version_32 := 16#b46168d5#;
   pragma Export (C, u00076, "ada__io_exceptionsS");
   u00077 : constant Version_32 := 16#e4d3df20#;
   pragma Export (C, u00077, "interfaces__c__stringsB");
   u00078 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00078, "interfaces__c__stringsS");
   u00079 : constant Version_32 := 16#a50435f4#;
   pragma Export (C, u00079, "system__crtl__runtimeS");
   u00080 : constant Version_32 := 16#f4d04ad4#;
   pragma Export (C, u00080, "system__os_libB");
   u00081 : constant Version_32 := 16#a6d80a38#;
   pragma Export (C, u00081, "system__os_libS");
   u00082 : constant Version_32 := 16#4cd8aca0#;
   pragma Export (C, u00082, "system__stringsB");
   u00083 : constant Version_32 := 16#5c84087e#;
   pragma Export (C, u00083, "system__stringsS");
   u00084 : constant Version_32 := 16#3451ac80#;
   pragma Export (C, u00084, "system__file_control_blockS");
   u00085 : constant Version_32 := 16#6d35da9a#;
   pragma Export (C, u00085, "system__finalization_mastersB");
   u00086 : constant Version_32 := 16#819bee96#;
   pragma Export (C, u00086, "system__finalization_mastersS");
   u00087 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00087, "system__address_imageB");
   u00088 : constant Version_32 := 16#4a82df80#;
   pragma Export (C, u00088, "system__address_imageS");
   u00089 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00089, "system__img_boolB");
   u00090 : constant Version_32 := 16#1eb73351#;
   pragma Export (C, u00090, "system__img_boolS");
   u00091 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00091, "system__ioB");
   u00092 : constant Version_32 := 16#752cb5f5#;
   pragma Export (C, u00092, "system__ioS");
   u00093 : constant Version_32 := 16#a7a37cb6#;
   pragma Export (C, u00093, "system__storage_poolsB");
   u00094 : constant Version_32 := 16#38c05dd7#;
   pragma Export (C, u00094, "system__storage_poolsS");
   u00095 : constant Version_32 := 16#ba5d60c7#;
   pragma Export (C, u00095, "system__pool_globalB");
   u00096 : constant Version_32 := 16#d56df0a6#;
   pragma Export (C, u00096, "system__pool_globalS");
   u00097 : constant Version_32 := 16#733fc7cf#;
   pragma Export (C, u00097, "system__memoryB");
   u00098 : constant Version_32 := 16#21e5feaf#;
   pragma Export (C, u00098, "system__memoryS");
   u00099 : constant Version_32 := 16#17551a52#;
   pragma Export (C, u00099, "system__storage_pools__subpoolsB");
   u00100 : constant Version_32 := 16#738e4bc9#;
   pragma Export (C, u00100, "system__storage_pools__subpoolsS");
   u00101 : constant Version_32 := 16#a19377ea#;
   pragma Export (C, u00101, "logB");
   u00102 : constant Version_32 := 16#14beb0c4#;
   pragma Export (C, u00102, "logS");
   u00103 : constant Version_32 := 16#7a13e6d7#;
   pragma Export (C, u00103, "ada__calendar__formattingB");
   u00104 : constant Version_32 := 16#929f882b#;
   pragma Export (C, u00104, "ada__calendar__formattingS");
   u00105 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00105, "ada__calendar__time_zonesB");
   u00106 : constant Version_32 := 16#98f012d7#;
   pragma Export (C, u00106, "ada__calendar__time_zonesS");
   u00107 : constant Version_32 := 16#7993dbbd#;
   pragma Export (C, u00107, "system__val_intB");
   u00108 : constant Version_32 := 16#a3cb6885#;
   pragma Export (C, u00108, "system__val_intS");
   u00109 : constant Version_32 := 16#e6965fe6#;
   pragma Export (C, u00109, "system__val_unsB");
   u00110 : constant Version_32 := 16#9127f3f7#;
   pragma Export (C, u00110, "system__val_unsS");
   u00111 : constant Version_32 := 16#730c1f82#;
   pragma Export (C, u00111, "system__val_realB");
   u00112 : constant Version_32 := 16#154735ab#;
   pragma Export (C, u00112, "system__val_realS");
   u00113 : constant Version_32 := 16#0be1b996#;
   pragma Export (C, u00113, "system__exn_llfB");
   u00114 : constant Version_32 := 16#6aea5c55#;
   pragma Export (C, u00114, "system__exn_llfS");
   u00115 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00115, "system__float_controlB");
   u00116 : constant Version_32 := 16#0b920186#;
   pragma Export (C, u00116, "system__float_controlS");
   u00117 : constant Version_32 := 16#bb1e24cd#;
   pragma Export (C, u00117, "system__powten_tableS");
   u00118 : constant Version_32 := 16#6cdeae02#;
   pragma Export (C, u00118, "ada__real_timeB");
   u00119 : constant Version_32 := 16#41de19c7#;
   pragma Export (C, u00119, "ada__real_timeS");
   u00120 : constant Version_32 := 16#93d8ec4d#;
   pragma Export (C, u00120, "system__arith_64B");
   u00121 : constant Version_32 := 16#59e6c039#;
   pragma Export (C, u00121, "system__arith_64S");
   u00122 : constant Version_32 := 16#ccec2ba2#;
   pragma Export (C, u00122, "system__taskingB");
   u00123 : constant Version_32 := 16#3f81803c#;
   pragma Export (C, u00123, "system__taskingS");
   u00124 : constant Version_32 := 16#6d133e11#;
   pragma Export (C, u00124, "system__task_primitivesS");
   u00125 : constant Version_32 := 16#e0522444#;
   pragma Export (C, u00125, "system__os_interfaceB");
   u00126 : constant Version_32 := 16#35faef2d#;
   pragma Export (C, u00126, "system__os_interfaceS");
   u00127 : constant Version_32 := 16#9ba989ff#;
   pragma Export (C, u00127, "system__linuxS");
   u00128 : constant Version_32 := 16#2f5dc06d#;
   pragma Export (C, u00128, "system__os_constantsS");
   u00129 : constant Version_32 := 16#b4921ea2#;
   pragma Export (C, u00129, "system__task_primitives__operationsB");
   u00130 : constant Version_32 := 16#73696aed#;
   pragma Export (C, u00130, "system__task_primitives__operationsS");
   u00131 : constant Version_32 := 16#9ffb82dd#;
   pragma Export (C, u00131, "system__bit_opsB");
   u00132 : constant Version_32 := 16#c30e4013#;
   pragma Export (C, u00132, "system__bit_opsS");
   u00133 : constant Version_32 := 16#903909a4#;
   pragma Export (C, u00133, "system__interrupt_managementB");
   u00134 : constant Version_32 := 16#6b7e2624#;
   pragma Export (C, u00134, "system__interrupt_managementS");
   u00135 : constant Version_32 := 16#c313b593#;
   pragma Export (C, u00135, "system__multiprocessorsB");
   u00136 : constant Version_32 := 16#d3c2ddc9#;
   pragma Export (C, u00136, "system__multiprocessorsS");
   u00137 : constant Version_32 := 16#55faabf1#;
   pragma Export (C, u00137, "system__stack_checking__operationsB");
   u00138 : constant Version_32 := 16#49df1cef#;
   pragma Export (C, u00138, "system__stack_checking__operationsS");
   u00139 : constant Version_32 := 16#3d54d5f6#;
   pragma Export (C, u00139, "system__task_infoB");
   u00140 : constant Version_32 := 16#59d440ea#;
   pragma Export (C, u00140, "system__task_infoS");
   u00141 : constant Version_32 := 16#2a6dd755#;
   pragma Export (C, u00141, "system__tasking__debugB");
   u00142 : constant Version_32 := 16#8c562538#;
   pragma Export (C, u00142, "system__tasking__debugS");
   u00143 : constant Version_32 := 16#39591e91#;
   pragma Export (C, u00143, "system__concat_2B");
   u00144 : constant Version_32 := 16#10beb046#;
   pragma Export (C, u00144, "system__concat_2S");
   u00145 : constant Version_32 := 16#ae97ef6c#;
   pragma Export (C, u00145, "system__concat_3B");
   u00146 : constant Version_32 := 16#9d4440d0#;
   pragma Export (C, u00146, "system__concat_3S");
   u00147 : constant Version_32 := 16#c9fdc962#;
   pragma Export (C, u00147, "system__concat_6B");
   u00148 : constant Version_32 := 16#2ca4b7ae#;
   pragma Export (C, u00148, "system__concat_6S");
   u00149 : constant Version_32 := 16#def1dd00#;
   pragma Export (C, u00149, "system__concat_5B");
   u00150 : constant Version_32 := 16#fb578c1b#;
   pragma Export (C, u00150, "system__concat_5S");
   u00151 : constant Version_32 := 16#3493e6c0#;
   pragma Export (C, u00151, "system__concat_4B");
   u00152 : constant Version_32 := 16#e931a104#;
   pragma Export (C, u00152, "system__concat_4S");
   u00153 : constant Version_32 := 16#1eab0e09#;
   pragma Export (C, u00153, "system__img_enum_newB");
   u00154 : constant Version_32 := 16#6c69894a#;
   pragma Export (C, u00154, "system__img_enum_newS");
   u00155 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00155, "system__img_lliB");
   u00156 : constant Version_32 := 16#fa21176b#;
   pragma Export (C, u00156, "system__img_lliS");
   u00157 : constant Version_32 := 16#06417083#;
   pragma Export (C, u00157, "system__img_lluB");
   u00158 : constant Version_32 := 16#c8461e0f#;
   pragma Export (C, u00158, "system__img_lluS");
   u00159 : constant Version_32 := 16#7b8aedca#;
   pragma Export (C, u00159, "system__stack_usageB");
   u00160 : constant Version_32 := 16#a5188558#;
   pragma Export (C, u00160, "system__stack_usageS");
   u00161 : constant Version_32 := 16#547bccfd#;
   pragma Export (C, u00161, "ialS");
   u00162 : constant Version_32 := 16#713738e0#;
   pragma Export (C, u00162, "ial__logB");
   u00163 : constant Version_32 := 16#ac7a498d#;
   pragma Export (C, u00163, "ial__logS");
   u00164 : constant Version_32 := 16#4d2c425e#;
   pragma Export (C, u00164, "name_serverS");
   u00165 : constant Version_32 := 16#f85ea1d6#;
   pragma Export (C, u00165, "system__tasking__protected_objectsB");
   u00166 : constant Version_32 := 16#0e06b2d3#;
   pragma Export (C, u00166, "system__tasking__protected_objectsS");
   u00167 : constant Version_32 := 16#695e2a32#;
   pragma Export (C, u00167, "system__soft_links__taskingB");
   u00168 : constant Version_32 := 16#6ac0d6d0#;
   pragma Export (C, u00168, "system__soft_links__taskingS");
   u00169 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00169, "ada__exceptions__is_null_occurrenceB");
   u00170 : constant Version_32 := 16#24d5007b#;
   pragma Export (C, u00170, "ada__exceptions__is_null_occurrenceS");
   u00171 : constant Version_32 := 16#e844a3d9#;
   pragma Export (C, u00171, "system__tasking__protected_objects__entriesB");
   u00172 : constant Version_32 := 16#db92b260#;
   pragma Export (C, u00172, "system__tasking__protected_objects__entriesS");
   u00173 : constant Version_32 := 16#386436bc#;
   pragma Export (C, u00173, "system__restrictionsB");
   u00174 : constant Version_32 := 16#a7a3f233#;
   pragma Export (C, u00174, "system__restrictionsS");
   u00175 : constant Version_32 := 16#176ce286#;
   pragma Export (C, u00175, "system__tasking__initializationB");
   u00176 : constant Version_32 := 16#93a57cc9#;
   pragma Export (C, u00176, "system__tasking__initializationS");
   u00177 : constant Version_32 := 16#c468a124#;
   pragma Export (C, u00177, "system__tasking__protected_objects__operationsB");
   u00178 : constant Version_32 := 16#c3da2e0f#;
   pragma Export (C, u00178, "system__tasking__protected_objects__operationsS");
   u00179 : constant Version_32 := 16#ee28ed55#;
   pragma Export (C, u00179, "system__tasking__entry_callsB");
   u00180 : constant Version_32 := 16#84b0eb9c#;
   pragma Export (C, u00180, "system__tasking__entry_callsS");
   u00181 : constant Version_32 := 16#385ecace#;
   pragma Export (C, u00181, "system__tasking__queuingB");
   u00182 : constant Version_32 := 16#ca5254e7#;
   pragma Export (C, u00182, "system__tasking__queuingS");
   u00183 : constant Version_32 := 16#5270cf31#;
   pragma Export (C, u00183, "system__tasking__utilitiesB");
   u00184 : constant Version_32 := 16#588eda2e#;
   pragma Export (C, u00184, "system__tasking__utilitiesS");
   u00185 : constant Version_32 := 16#bd6fc52e#;
   pragma Export (C, u00185, "system__traces__taskingB");
   u00186 : constant Version_32 := 16#52029525#;
   pragma Export (C, u00186, "system__traces__taskingS");
   u00187 : constant Version_32 := 16#5a8b2f09#;
   pragma Export (C, u00187, "system__tasking__rendezvousB");
   u00188 : constant Version_32 := 16#34f28e26#;
   pragma Export (C, u00188, "system__tasking__rendezvousS");
   u00189 : constant Version_32 := 16#1d50dbf5#;
   pragma Export (C, u00189, "system__tasking__stagesB");
   u00190 : constant Version_32 := 16#9022d0bb#;
   pragma Export (C, u00190, "system__tasking__stagesS");
   u00191 : constant Version_32 := 16#d646ea8d#;
   pragma Export (C, u00191, "name_server__configurationB");
   u00192 : constant Version_32 := 16#2d5c01e1#;
   pragma Export (C, u00192, "name_server__configurationS");
   u00193 : constant Version_32 := 16#2c2cb25a#;
   pragma Export (C, u00193, "ada__command_lineB");
   u00194 : constant Version_32 := 16#df5044bd#;
   pragma Export (C, u00194, "ada__command_lineS");
   u00195 : constant Version_32 := 16#6bf7d6c0#;
   pragma Export (C, u00195, "ial__propertiesB");
   u00196 : constant Version_32 := 16#3733eb5e#;
   pragma Export (C, u00196, "ial__propertiesS");
   u00197 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00197, "ada__charactersS");
   u00198 : constant Version_32 := 16#6239f067#;
   pragma Export (C, u00198, "ada__characters__handlingB");
   u00199 : constant Version_32 := 16#3006d996#;
   pragma Export (C, u00199, "ada__characters__handlingS");
   u00200 : constant Version_32 := 16#051b1b7b#;
   pragma Export (C, u00200, "ada__characters__latin_1S");
   u00201 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00201, "ada__stringsS");
   u00202 : constant Version_32 := 16#96e9c1e7#;
   pragma Export (C, u00202, "ada__strings__mapsB");
   u00203 : constant Version_32 := 16#24318e4c#;
   pragma Export (C, u00203, "ada__strings__mapsS");
   u00204 : constant Version_32 := 16#7a69aa90#;
   pragma Export (C, u00204, "ada__strings__maps__constantsS");
   u00205 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00205, "ada__containersS");
   u00206 : constant Version_32 := 16#654e2c4c#;
   pragma Export (C, u00206, "ada__containers__hash_tablesS");
   u00207 : constant Version_32 := 16#c24eaf4d#;
   pragma Export (C, u00207, "ada__containers__prime_numbersB");
   u00208 : constant Version_32 := 16#6d3af8ed#;
   pragma Export (C, u00208, "ada__containers__prime_numbersS");
   u00209 : constant Version_32 := 16#914b496f#;
   pragma Export (C, u00209, "ada__strings__fixedB");
   u00210 : constant Version_32 := 16#dc686502#;
   pragma Export (C, u00210, "ada__strings__fixedS");
   u00211 : constant Version_32 := 16#00363e01#;
   pragma Export (C, u00211, "ada__strings__searchB");
   u00212 : constant Version_32 := 16#b5a8c1d6#;
   pragma Export (C, u00212, "ada__strings__searchS");
   u00213 : constant Version_32 := 16#bd084245#;
   pragma Export (C, u00213, "ada__strings__hashB");
   u00214 : constant Version_32 := 16#fe83f2e7#;
   pragma Export (C, u00214, "ada__strings__hashS");
   u00215 : constant Version_32 := 16#d1ef3e3a#;
   pragma Export (C, u00215, "system__assertionsB");
   u00216 : constant Version_32 := 16#48caa411#;
   pragma Export (C, u00216, "system__assertionsS");
   u00217 : constant Version_32 := 16#a6e358bc#;
   pragma Export (C, u00217, "system__stream_attributesB");
   u00218 : constant Version_32 := 16#e89b4b3f#;
   pragma Export (C, u00218, "system__stream_attributesS");
   u00219 : constant Version_32 := 16#1eadf3c6#;
   pragma Export (C, u00219, "system__strings__stream_opsB");
   u00220 : constant Version_32 := 16#8453d1c6#;
   pragma Export (C, u00220, "system__strings__stream_opsS");
   u00221 : constant Version_32 := 16#2753da19#;
   pragma Export (C, u00221, "ada__streams__stream_ioB");
   u00222 : constant Version_32 := 16#f0e417a0#;
   pragma Export (C, u00222, "ada__streams__stream_ioS");
   u00223 : constant Version_32 := 16#595ba38f#;
   pragma Export (C, u00223, "system__communicationB");
   u00224 : constant Version_32 := 16#6940ec90#;
   pragma Export (C, u00224, "system__communicationS");
   u00225 : constant Version_32 := 16#5c0e4566#;
   pragma Export (C, u00225, "system__val_boolB");
   u00226 : constant Version_32 := 16#11fc2ea4#;
   pragma Export (C, u00226, "system__val_boolS");
   u00227 : constant Version_32 := 16#93c6386a#;
   pragma Export (C, u00227, "name_server__messagingB");
   u00228 : constant Version_32 := 16#36d46bbb#;
   pragma Export (C, u00228, "name_server__messagingS");
   u00229 : constant Version_32 := 16#50ea1220#;
   pragma Export (C, u00229, "ial__taskingS");
   u00230 : constant Version_32 := 16#07bef36f#;
   pragma Export (C, u00230, "ial__tasking__starterB");
   u00231 : constant Version_32 := 16#b2d68e75#;
   pragma Export (C, u00231, "ial__tasking__starterS");
   u00232 : constant Version_32 := 16#57463e63#;
   pragma Export (C, u00232, "name_server__cacheB");
   u00233 : constant Version_32 := 16#da080d12#;
   pragma Export (C, u00233, "name_server__cacheS");
   u00234 : constant Version_32 := 16#6cc01289#;
   pragma Export (C, u00234, "name_server__storageB");
   u00235 : constant Version_32 := 16#0838fea6#;
   pragma Export (C, u00235, "name_server__storageS");
   u00236 : constant Version_32 := 16#3e4e6762#;
   pragma Export (C, u00236, "ada__directoriesB");
   u00237 : constant Version_32 := 16#9c33e8ea#;
   pragma Export (C, u00237, "ada__directoriesS");
   u00238 : constant Version_32 := 16#e559f18d#;
   pragma Export (C, u00238, "ada__directories__validityB");
   u00239 : constant Version_32 := 16#a2334639#;
   pragma Export (C, u00239, "ada__directories__validityS");
   u00240 : constant Version_32 := 16#261c554b#;
   pragma Export (C, u00240, "ada__strings__unboundedB");
   u00241 : constant Version_32 := 16#2bf53506#;
   pragma Export (C, u00241, "ada__strings__unboundedS");
   u00242 : constant Version_32 := 16#c4857ee1#;
   pragma Export (C, u00242, "system__compare_array_unsigned_8B");
   u00243 : constant Version_32 := 16#3155b477#;
   pragma Export (C, u00243, "system__compare_array_unsigned_8S");
   u00244 : constant Version_32 := 16#9d3d925a#;
   pragma Export (C, u00244, "system__address_operationsB");
   u00245 : constant Version_32 := 16#2b10ab2d#;
   pragma Export (C, u00245, "system__address_operationsS");
   u00246 : constant Version_32 := 16#3b8ad87b#;
   pragma Export (C, u00246, "system__atomic_countersB");
   u00247 : constant Version_32 := 16#a1f22e0e#;
   pragma Export (C, u00247, "system__atomic_countersS");
   u00248 : constant Version_32 := 16#e7698cad#;
   pragma Export (C, u00248, "system__regexpB");
   u00249 : constant Version_32 := 16#25ffb906#;
   pragma Export (C, u00249, "system__regexpS");
   u00250 : constant Version_32 := 16#5cafead5#;
   pragma Export (C, u00250, "yamiS");
   u00251 : constant Version_32 := 16#c2cfe9ba#;
   pragma Export (C, u00251, "yami__coreS");
   u00252 : constant Version_32 := 16#1c396f71#;
   pragma Export (C, u00252, "yami__detailsB");
   u00253 : constant Version_32 := 16#6455ae1f#;
   pragma Export (C, u00253, "yami__detailsS");
   u00254 : constant Version_32 := 16#cf1307d2#;
   pragma Export (C, u00254, "yami__core__agentsB");
   u00255 : constant Version_32 := 16#9276bed9#;
   pragma Export (C, u00255, "yami__core__agentsS");
   u00256 : constant Version_32 := 16#be02383a#;
   pragma Export (C, u00256, "yami__core__closed_connection_handlersS");
   u00257 : constant Version_32 := 16#1b536a3f#;
   pragma Export (C, u00257, "yami__core__event_notification_handlersS");
   u00258 : constant Version_32 := 16#58a2a8de#;
   pragma Export (C, u00258, "yami__core__incoming_message_handlersS");
   u00259 : constant Version_32 := 16#5b03f390#;
   pragma Export (C, u00259, "yami__core__message_progress_handlersS");
   u00260 : constant Version_32 := 16#831be1ca#;
   pragma Export (C, u00260, "yami__core__new_connection_handlersS");
   u00261 : constant Version_32 := 16#1a578073#;
   pragma Export (C, u00261, "yami__parametersB");
   u00262 : constant Version_32 := 16#0a133840#;
   pragma Export (C, u00262, "yami__parametersS");
   u00263 : constant Version_32 := 16#0dbfd559#;
   pragma Export (C, u00263, "yami__serializablesB");
   u00264 : constant Version_32 := 16#781bf4b5#;
   pragma Export (C, u00264, "yami__serializablesS");
   u00265 : constant Version_32 := 16#40d6cb6f#;
   pragma Export (C, u00265, "yami__raw_buffer_data_sourcesB");
   u00266 : constant Version_32 := 16#2f916f08#;
   pragma Export (C, u00266, "yami__raw_buffer_data_sourcesS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.command_line%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.arith_64%s
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.linux%s
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_checking.operations%s
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.arith_64%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.stack_checking.operations%b
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.val_bool%s
   --  system.val_int%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_int%b
   --  system.val_bool%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.containers.hash_tables%s
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.hash%s
   --  ada.strings.hash%b
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.strings%s
   --  system.crtl.runtime%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.interrupt_management%b
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  system.assertions%s
   --  system.assertions%b
   --  system.communication%s
   --  system.communication%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  ada.streams.stream_io%s
   --  system.file_io%s
   --  ada.streams.stream_io%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  ada.command_line%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  ada.calendar.formatting%b
   --  system.address_image%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  ada.directories%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.file_io%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.tasking.entry_calls%s
   --  system.tasking.initialization%s
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.utilities%s
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%b
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.rendezvous%b
   --  system.tasking.entry_calls%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  ial%s
   --  ial.tasking%s
   --  name_server%s
   --  yami%s
   --  ial.log%s
   --  ial.log%b
   --  ial.properties%s
   --  ial.properties%b
   --  ial.tasking.starter%s
   --  ial.tasking.starter%b
   --  log%s
   --  log%b
   --  name_server.cache%s
   --  name_server.cache%b
   --  name_server.configuration%s
   --  name_server.configuration%b
   --  name_server.storage%s
   --  name_server.storage%b
   --  yami.details%s
   --  yami.details%b
   --  yami.core%s
   --  yami.core.closed_connection_handlers%s
   --  yami.core.event_notification_handlers%s
   --  yami.core.incoming_message_handlers%s
   --  yami.core.message_progress_handlers%s
   --  yami.core.new_connection_handlers%s
   --  yami.serializables%s
   --  yami.serializables%b
   --  yami.parameters%s
   --  yami.parameters%b
   --  yami.raw_buffer_data_sources%s
   --  yami.raw_buffer_data_sources%b
   --  yami.core.agents%s
   --  yami.core.agents%b
   --  name_server.messaging%s
   --  name_server.messaging%b
   --  yami4names%b
   --  END ELABORATION ORDER


end ada_main;

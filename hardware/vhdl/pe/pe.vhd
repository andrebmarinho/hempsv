------------------------------------------------------------------------------------------------
-- HeMPS Processing Element
------------------------------------------------------------------------------------------------
library ieee;
--use work.mlite_pack.all;                
use work.standards.all;
use work.hemps_pkg.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_textio.all;
use ieee.std_logic_unsigned.all;
use ieee.math_real.all;
use std.textio.all;

entity pe is
    generic 
    (
        log_file            : string := "output.txt";
        ram_file            : string := "ram.txt";
        router_address      : regmetadeflit:= (others=>'0');
        kernel_type			: kernel_str
    );
    port 
    (  
    	clock                   : in  std_logic;
        reset                   : in  std_logic;

        cpu_number              : in integer;

		-- NoC
        clock_rx                : in  std_logic_vector(3 downto 0);
        rx                      : in  std_logic_vector(3 downto 0);
        data_in                 : in  arrayNPORT_1_regflit;
        credit_o                : out std_logic_vector(3 downto 0);
        clock_tx                : out std_logic_vector(3 downto 0);
        tx                      : out std_logic_vector(3 downto 0);
        data_out                : out arrayNPORT_1_regflit;
        credit_i                : in  std_logic_vector(3 downto 0);
        -- External Memory
        repo_address                 : out std_logic_vector(29 downto 0);
        repo_data_read               : in  std_logic_vector(31 downto 0);
        ack_app                 : out  std_logic;
        req_app                 : in  std_logic_vector(31 downto 0)
        -- External Debug interface
--        write_enable_debug      : out  std_logic;
--        data_out_debug          : out  std_logic_vector(31 downto 0);
--        busy_debug              : in std_logic
    );
end entity pe;

architecture structural of pe is
    constant ZERO          : std_logic_vector(31 downto 0) :=
        "00000000000000000000000000000000";
    constant ONES          : std_logic_vector(31 downto 0) :=
        "11111111111111111111111111111111";
    constant HIGH_Z        : std_logic_vector(31 downto 0) :=
        "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";

	type repo_state is (WAIT_state, COPY_FROM_REP);
    signal repo_FSM        : repo_state;
	    
    signal irq_mask_reg                  : std_logic_vector(7 downto 0);
    signal irq_status                    : std_logic_vector(7 downto 0);
    signal irq                           : std_logic;

    signal irq_ret  : std_logic;

    signal time_slice                    : std_logic_vector(31 downto 0);
    signal write_enable                  : std_logic;
    signal tick_counter_local            : std_logic_vector(31 downto 0);
    signal tick_counter                  : std_logic_vector(31 downto 0);
    signal current_page                  : std_logic_vector(7 downto 0);
        
    signal cpu_enable_ram                : std_logic;
    signal cpu_set_size                  : std_logic;
    signal cpu_set_address               : std_logic;
    signal cpu_set_size_2                : std_logic;
    signal cpu_set_address_2             : std_logic;
    signal cpu_set_op                    : std_logic;
    signal cpu_start                     : std_logic;
    signal clock_aux                     : std_logic;
    signal clock_hold_s                  : std_logic;
    signal pending_service               : std_logic;
    
    --Router
    signal clock_rx_router               : regNport;
    signal rx_router                     : regNport;
    signal data_in_router                : arrayNport_regflit;
    signal credit_o_router               : regNport;
    signal clock_tx_router               : regNport;
    signal tx_router                     : regNport;
    signal data_out_router               : arrayNport_regflit;
    signal credit_i_router               : regNport;
    
    signal data_read_ram                 : std_logic_vector(31 downto 0);
    signal mem_data_read                 : std_logic_vector(31 downto 0);
    --signal debug_busy                    : std_logic;
    --signal debug_write_data              : std_logic;
    --signal debug_write_busy              : std_logic;
    --signal debug_data_avail              : std_logic;
    signal ni_intr                       : std_logic;
    signal dmni_mem_address              : std_logic_vector(31 downto 0);
    signal dmni_mem_write_byte_enable    : std_logic_vector(3 downto 0);
    signal dmni_mem_data_write           : std_logic_vector(31 downto 0);
    signal dmni_mem_data_read            : std_logic_vector(31 downto 0);
    signal dmni_data_read                : std_logic_vector(31 downto 0);
    signal dmni_enable_internal_ram      : std_logic;
    signal dmni_send_active_sig          : std_logic;
    signal dmni_receive_active_sig       : std_logic;
    signal addr_dmni                       : std_logic_vector(31 downto 2);
    signal cpu_repo_access               : std_logic := '0';
    signal data_av                       : std_logic;
    signal end_sim_reg                   : std_logic_vector(31 downto 0);
    signal uart_write_data               : std_logic;
    
    signal slack_update_timer			  : std_logic_vector(31 downto 0);

    --original CPU signals
    -- signal cpu_mem_address_reg           : std_logic_vector(31 downto 0);
    -- signal cpu_mem_data_write_reg        : std_logic_vector(31 downto 0);
    -- signal cpu_mem_write_byte_enable_reg : std_logic_vector(3 downto 0);
    --signal cpu_mem_address               : std_logic_vector(31 downto 0);
    -- signal cpu_mem_data_write            : std_logic_vector(31 downto 0);
    -- signal cpu_mem_data_read             : std_logic_vector(31 downto 0);
    -- signal cpu_mem_write_byte_enable     : std_logic_vector(3 downto 0);
    signal cpu_mem_pause                 : std_logic;

    --Clarvi aux
    -- Regs
        -- Addresses
    signal cpu_mem_data_address_reg             : std_logic_vector(31 downto 0);
    signal cpu_mem_instr_address_reg            : std_logic_vector(31 downto 0);    

        -- Data to be written
    signal cpu_mem_data_write_reg               : std_logic_vector(31 downto 0);
    signal cpu_mem_data_write_reg_big           : std_logic_vector(31 downto 0);
    signal cpu_mem_instr_write_reg              : std_logic_vector(31 downto 0);
    
        -- Write enable
    signal cpu_mem_data_write_byte_enable_reg : std_logic_vector(3 downto 0);
    signal cpu_mem_instr_write_byte_enable_reg : std_logic_vector(3 downto 0);    
    signal wbe_a                                : std_logic_vector(3 downto 0); 

    -- Addresses
    signal cpu_mem_data_address_middle          : std_logic_vector(28 downto 0);
    signal cpu_mem_data_address                 : std_logic_vector(31 downto 0);
    signal cpu_mem_data_address_paginated       : std_logic_vector(31 downto 0);
    signal address_a                            : std_logic_vector(31 downto 2);

    signal cpu_mem_instr_address_middle         : std_logic_vector(28 downto 0);
    signal cpu_mem_instr_address                : std_logic_vector(31 downto 0);
    signal cpu_mem_instr_address_paginated      : std_logic_vector(31 downto 0);

    -- Data to be written
    signal cpu_mem_data_write                   : std_logic_vector(31 downto 0);
    signal cpu_mem_instr_write                  : std_logic_vector(31 downto 0);
    signal data_write_a                         : std_logic_vector(31 downto 0);

    -- Data to be read
    signal cpu_mem_data_read                    : std_logic_vector(31 downto 0);
    signal cpu_mem_data_read_from_ram           : std_logic_vector(31 downto 0);
    signal cpu_mem_instr_read                   : std_logic_vector(31 downto 0);
    signal data_read_a                          : std_logic_vector(31 downto 0);
    

    signal reading_data                         : std_logic;
    signal reading_data_reg                     : std_logic;

    -- Write enable
    signal cpu_mem_data_write_byte_enable       : std_logic_vector(3 downto 0);
    signal cpu_mem_instr_write_byte_enable      : std_logic_vector(3 downto 0);

    -- Auxiliary signal to paginate on the wrapper
    signal page_to_wrapper                      : std_logic_vector(31 downto 0);

    -- Auxiliary signal to set page to 0
    signal ecall                                : std_logic;

    -- signal data_writedata   : std_logic_vector(31 downto 0);
    -- signal data_readdata    : std_logic_vector(31 downto 0);

    -- signal instr_readdata   : std_logic_vector(31 downto 0);
    -- signal instr_writedata  : std_logic_vector(31 downto 0);
    -- signal instr_address    : std_logic_vector(28 downto 0);

    -- signal wbe_instr        : std_logic_vector(3 downto 0);
    -- signal wbe_data         : std_logic_vector(3 downto 0);

    -- Pagination - Clarvi
    signal change_page      : std_logic;
    signal next_page_reg    : std_logic_vector(31 downto 0);

    -- DEBUG messages - Clarvi
    signal trace            : std_logic := '1';

    -- With new stages, there's a necessity of controlling ack and repo address:
    signal invalidate_ack : std_logic := '0';
    
begin
	
-----------------------------------------------------------------------------------
-- PE COMPONENTS INSTANTIATION
-----------------------------------------------------------------------------------
	-- cpu : entity work.mlite_cpu
	-- 	port map(
	-- 		clk          => clock_hold_s,
	-- 		reset_in     => reset,
	-- 		intr_in      => irq,
	-- 		mem_address  => cpu_mem_address,
	-- 		mem_data_w   => cpu_mem_data_write,
	-- 		mem_data_r   => cpu_mem_data_read,
	-- 		mem_byte_we  => cpu_mem_write_byte_enable,
	-- 		mem_pause    => cpu_mem_pause,
	-- 		current_page => current_page
	-- 	);

    --trace <= '0' when kernel_type = "mas" else '1';   

    cpu: entity work.clarvi_wrapper
		generic map (
			ADDR_WIDTH          => 29,
			PAGE_NUMBER_H_INDEX => PAGE_NUMBER_H_INDEX,
            PAGE_SIZE_H_INDEX   => PAGE_SIZE_H_INDEX
		) port map (
            cpu_number          => cpu_number,                 
            trace               => trace,                 
			clock               => clock,                             	
			reset               => reset,		                            	
			interrupt           => irq,                          
			interrupt_ret       => irq_ret,
            -- Addresses
			data_address        => cpu_mem_data_address,              
            instr_address       => cpu_mem_instr_address,             
                                                                        
            -- Data to be written
            data_writedata      => cpu_mem_data_write,                      
			instr_writedata     => cpu_mem_instr_write,                     

            -- Data to be read
			data_readdata       => cpu_mem_data_read,                       
			instr_readdata      => cpu_mem_instr_read,                      
			
            cpu_mem_pause       => cpu_mem_pause,

            -- Write enables
			wbe_data            => cpu_mem_data_write_byte_enable,     				
			wbe_instr           => cpu_mem_instr_write_byte_enable,
			
            page                => page_to_wrapper,           
            ecall               => ecall                  
		);

    -- current_page <= page(PAGE_SIZE_H_INDEX+8 downto PAGE_SIZE_H_INDEX+1);
    
    page_to_wrapper(31 downto PAGE_SIZE_H_INDEX+9) <= (others => '0');
    page_to_wrapper(PAGE_SIZE_H_INDEX+8 downto PAGE_SIZE_H_INDEX+1) <= current_page; 
    page_to_wrapper(PAGE_SIZE_H_INDEX downto 0) <= (others => '0');

    -- Abordagem de ram com 3 portas 
    -- mem: entity work.ram
	-- 	generic map(
	-- 		ram_file => ram_file        
	-- 	) port map (
	-- 		clk 						=> clock,

    --         -- Data
    --         wbe_a						=> cpu_mem_data_write_byte_enable,
	-- 		address_a		            => cpu_mem_data_address_paginated(31 downto 2), 
    --         data_write_a				=> cpu_mem_data_write,--cpu_mem_data_write,    
	-- 		data_read_a    				=> cpu_mem_data_read_from_ram, --data_read_ram,

    --         -- Instructions
	-- 		wbe_b           			=> cpu_mem_instr_write_byte_enable,
    --         address_b                   => cpu_mem_instr_address_paginated(31 downto 2),  
	-- 		data_write_b    			=> cpu_mem_instr_write,--x"00000000",
	-- 		data_read_b     			=> cpu_mem_instr_read,

    --         -- DMNI Interface...
	-- 		wbe_c           			=> dmni_mem_write_byte_enable, 
	-- 		address_c      				=> addr_dmni, 
	-- 		data_write_c    			=> dmni_mem_data_write,
	-- 		data_read_c    				=> mem_data_read
	-- 	);

    -- Abordagem de ram com 2 portas    
    address_a <= addr_dmni when dmni_send_active_sig = '1' or dmni_receive_active_sig = '1' else cpu_mem_data_address(31 downto 2);
   
    wbe_a <= dmni_mem_write_byte_enable when dmni_send_active_sig = '1' or dmni_receive_active_sig = '1' else cpu_mem_data_write_byte_enable;
    data_write_a <= dmni_mem_data_write when dmni_send_active_sig = '1' or dmni_receive_active_sig = '1' else cpu_mem_data_write;
  
    cpu_mem_data_read_from_ram <= data_read_a;-- when dmni_send_active_sig = '0' and dmni_receive_active_sig = '0' else x"00000000";
    mem_data_read <= data_read_a;-- when dmni_send_active_sig = '1' or dmni_receive_active_sig = '1' else x"00000000";

    mem: entity work.ram
		generic map(
			ram_file => ram_file        
		) port map (
			clk 						=> clock,

            -- Data/DMNI
            wbe_a					    => wbe_a,
			address_a		            => address_a,
            data_write_a				=> data_write_a,
			data_read_a    				=> data_read_a, 

            -- Instructions
			wbe_b           			=> cpu_mem_instr_write_byte_enable,
            address_b                   => cpu_mem_instr_address(31 downto 2),
			data_write_b    			=> cpu_mem_instr_write,--x"00000000",
			data_read_b     			=> cpu_mem_instr_read
		);

	router : Entity work.RouterCC
		generic map(address => router_address)
		port map(
			clock    => clock,
			reset    => reset,
			clock_rx => clock_rx_router,
			rx       => rx_router,
			data_in  => data_in_router,
			credit_o => credit_o_router,
			clock_tx => clock_tx_router,
			tx       => tx_router,
			data_out => data_out_router,
			credit_i => credit_i_router
		);

    dmni : entity work.dmni
    	generic map(
    		address_router => router_address
    	)
    	port map(
    		clock          => clock,
    		reset          => reset,
    		--Configuration interface
    		set_address    => cpu_set_address,
    		set_address_2  => cpu_set_address_2,
    		set_size       => cpu_set_size,
    		set_size_2     => cpu_set_size_2,
    		set_op         => cpu_set_op,
    		start          => cpu_start,
    		config_data    => dmni_data_read,

    		-- Status outputs
    		intr           => ni_intr,
    		send_active    => dmni_send_active_sig,
    		receive_active => dmni_receive_active_sig,

    		-- Memory interface
    		mem_address    => dmni_mem_address,
    		mem_data_write => dmni_mem_data_write,
    		mem_data_read  => dmni_mem_data_read,
    		mem_byte_we    => dmni_mem_write_byte_enable,

    		--NoC Interface (Local port)
    		tx             => rx_router(LOCAL),
    		data_out       => data_in_router(LOCAL),
    		credit_i       => credit_o_router(LOCAL),
    		clock_tx       => clock_rx_router(LOCAL),
    		rx             => tx_router(LOCAL),
    		data_in        => data_out_router(LOCAL),
    		credit_o       => credit_i_router(LOCAL),
    		clock_rx       => clock_tx_router(LOCAL)
    	);

    uart : entity work.UartFile
    	generic map(
    		log_file => log_file
    	)
    	port map(
    		reset   => reset,
    		data_av => uart_write_data,
    		-- data_in => cpu_mem_data_write_reg
    		data_in => cpu_mem_data_write_reg_big
    	);
    	
-----------------------------------------------------------------------------------
-- COMBINATIONAL LOGIC AND WIRING
-----------------------------------------------------------------------------------

    --Router external wiring
    clock_rx_router	(3 downto 0)	<= clock_rx;
    rx_router		(3 downto 0)	<= rx;
    credit_i_router	(3 downto 0)	<= credit_i;
    data_in_router	(EAST)			<= data_in(EAST);
    data_in_router	(WEST)			<= data_in(WEST);
    data_in_router	(NORTH)			<= data_in(NORTH);
    data_in_router	(SOUTH)			<= data_in(SOUTH);
    
    clock_tx					<= clock_tx_router	(3 downto 0);
    tx							<= tx_router		(3 downto 0);
    credit_o					<= credit_o_router	(3 downto 0);
    data_out(EAST)				<= data_out_router	(EAST);
    data_out(WEST)				<= data_out_router	(WEST);
    data_out(NORTH)				<= data_out_router	(NORTH);
    data_out(SOUTH)				<= data_out_router	(SOUTH);
    	
    -- UART 
    uart_write_data <= '1' when cpu_mem_data_address_reg = DEBUG and write_enable = '1' else '0';

    -- Since risc-v generates litte-endian data memory, we must convert it to big:
    cpu_mem_data_write_reg_big(31 downto 24) <= cpu_mem_data_write_reg(7 downto 0);
    cpu_mem_data_write_reg_big(23 downto 16) <= cpu_mem_data_write_reg(15 downto 8);
    cpu_mem_data_write_reg_big(15 downto 8) <= cpu_mem_data_write_reg(23 downto 16);
    cpu_mem_data_write_reg_big(7 downto 0) <= cpu_mem_data_write_reg(31 downto 24);

	-- CPU data read mux
    
    -- assert cpu_mem_data_read(31 downto 0) /= x"00000113" report "Err2" severity failure;
    
    -- assert cpu_mem_data_read(31 downto 0) /= x"00000113" report 

    -- select_repo <= '1' when cpu_mem_data_address_reg(30 downto 28) = "001" and reading_data = '0' else '0';

    MUX_CPU : 
        cpu_mem_data_read <=    repo_data_read                              when cpu_mem_data_address_reg(30 downto 28) = "001" else
                                -- x"AABBCCDD"                                  when cpu_mem_data_address_reg = IRQ_MASK else
                                ZERO(31 downto 8) & irq_mask_reg            when cpu_mem_data_address_reg = IRQ_MASK else
                                ZERO(31 downto 8) & irq_status              when cpu_mem_data_address_reg = IRQ_STATUS_ADDR else
                                time_slice                                  when cpu_mem_data_address_reg = TIME_SLICE_ADDR else
                                ZERO(31 downto 16) & router_address         when cpu_mem_data_address_reg = NET_ADDRESS else
                                tick_counter                                when cpu_mem_data_address_reg = TICK_COUNTER_ADDR else  
                                req_app                                     when cpu_mem_data_address_reg = REQ_APP_REG else                                 
                                ZERO(31 downto 1) & dmni_send_active_sig    when cpu_mem_data_address_reg = DMNI_SEND_ACTIVE else                                    
                                ZERO(31 downto 1) & dmni_receive_active_sig when cpu_mem_data_address_reg = DMNI_RECEIVE_ACTIVE else
                                next_page_reg                               when cpu_mem_data_address_reg = NEXT_PAGE else
                                cpu_mem_data_read_from_ram;

    -- external_cpu_mem_data_read <= repo_data_read; -- when cpu_mem_data_address_reg(30 downto 28) = "001"; 

    -- cpu_mem_data_read <= internal_cpu_mem_data_read when cpu_mem_data_address_reg(30 downto 28) /= "001"
    --                                                     and reading_data = '0' else
    --                      external_cpu_mem_data_read;

    --Expand CPU addresses
    --cpu_mem_data_address <= '0' & cpu_mem_data_address_middle(28 downto 0) & "00"; -- 00 ao fim indica um endereco alinhado
    --cpu_mem_instr_address <= '0' & cpu_mem_instr_address_middle(28 downto 0) & "00"; -- 00 ao fim indica um endereco alinhado

    -- Comb assignments
    -- cpu_mem_instr_address_paginated(31 downto 28)                    <= cpu_mem_instr_address(31 downto 28);
    -- cpu_mem_instr_address_paginated(27 downto PAGE_SIZE_H_INDEX + 1) <= ZERO(27 downto PAGE_SIZE_H_INDEX + 9) & current_page when current_page /= "00000000" and cpu_mem_instr_address(31 downto PAGE_SIZE_H_INDEX + 1) /= ZERO(31 downto PAGE_SIZE_H_INDEX + 1)
    -- 	                                                                                                    else cpu_mem_instr_address(27 downto PAGE_SIZE_H_INDEX + 1);
    -- cpu_mem_instr_address_paginated(PAGE_SIZE_H_INDEX downto 2)      <= cpu_mem_instr_address(PAGE_SIZE_H_INDEX downto 2);

    -- cpu_mem_data_address_paginated(31 downto 28)                    <= cpu_mem_data_address(31 downto 28);
    -- cpu_mem_data_address_paginated(27 downto PAGE_SIZE_H_INDEX + 1) <= ZERO(27 downto PAGE_SIZE_H_INDEX + 9) & current_page when current_page /= "00000000" and cpu_mem_data_address(31 downto PAGE_SIZE_H_INDEX + 1) /= ZERO(31 downto PAGE_SIZE_H_INDEX + 1)
    -- 	                                                                                                    else cpu_mem_data_address(27 downto PAGE_SIZE_H_INDEX + 1);
    -- cpu_mem_data_address_paginated(PAGE_SIZE_H_INDEX downto 2)      <= cpu_mem_data_address(PAGE_SIZE_H_INDEX downto 2);

    addr_dmni                <= dmni_mem_address(31 downto 2);
    -- -- data_av                  <= '1' when cpu_instr_address_reg = DEBUG and write_enable = '1' else '0';
    cpu_mem_pause            <= cpu_repo_access or dmni_receive_active_sig or dmni_send_active_sig;
    -- irq <= ni_intr;
    irq                      <= '1' when (irq_status /= x"00" and irq_mask_reg /= x"00") else '0';
    dmni_data_read           <= cpu_mem_data_write_reg;
    dmni_mem_data_read       <= mem_data_read when dmni_enable_internal_ram = '1' else repo_data_read;

    dmni_enable_internal_ram <= '1' when dmni_mem_address(30 downto 28) = "000" else '0';
    end_sim_reg              <= x"00000000" when cpu_mem_data_address_reg = END_SIM and write_enable = '1' else x"00000001";

    irq_status(7 downto 4)   <= "00" & ni_intr & '0';
    irq_status(3)            <= '1' when time_slice = x"00000001" else '0';
    irq_status(2)   		 <= '0';
    irq_status(1)   		 <= '1' when dmni_send_active_sig = '0' and slack_update_timer = SLACK_MONITOR_WINDOW else '0';
    irq_status(0)            <= (not dmni_send_active_sig and pending_service);

    cpu_set_address_2 <= '1' when cpu_mem_data_address_reg = DMNI_ADDR_2 and write_enable = '1' else '0';
    cpu_set_address   <= '1' when cpu_mem_data_address_reg = DMNI_ADDR and write_enable = '1' else '0';
    cpu_set_size      <= '1' when cpu_mem_data_address_reg = DMNI_SIZE and write_enable = '1' else '0';
    cpu_set_size_2    <= '1' when cpu_mem_data_address_reg = DMNI_SIZE_2 and write_enable = '1' else '0';

    -- DBG: regs final 05 e 15
    -- assert not(cpu_mem_data_address_reg = DMNI_SIZE_2 and write_enable = '1') report "DMNI_SIZE_2 Ok!";
    -- assert not(cpu_mem_data_address_reg = DMNI_ADDR_2 and write_enable = '1') report "DMNI_ADDR_2 Ok!";

    cpu_set_op        <= '1' when (cpu_mem_data_address_reg = DMNI_OP and write_enable = '1') else '0';
    cpu_start         <= '1' when (cpu_mem_data_address_reg = START_DMNI and write_enable = '1') else '0';
    
    write_enable <= '1' when cpu_mem_data_write_byte_enable_reg /= "0000" else '0';

-- -----------------------------------------------------------------------------------
-- -- SYNCHRONOUS PROCESSES
-- -----------------------------------------------------------------------------------

	repo_to_mem_access: process(clock,reset)
    begin
        if reset = '1' then
            repo_FSM <= WAIT_state;
            cpu_repo_access <= '0';
        elsif rising_edge(clock) then
            case( repo_FSM ) is            
                when WAIT_state =>
                    if(cpu_mem_data_address(30 downto 28) = "001") then
                        cpu_repo_access <= '1';
                        repo_FSM <= COPY_FROM_REP;
                    end if;
                when COPY_FROM_REP =>
                    repo_FSM <= WAIT_state;
                    cpu_repo_access <= '0';
            end case;
        end if;
    end process repo_to_mem_access;

    process(cpu_repo_access, dmni_mem_address)
    begin
        if(cpu_repo_access = '1') then 
            repo_address <= cpu_mem_data_address(29 downto 0) ;
        elsif dmni_mem_address(30 downto 28) = "001" then
            repo_address <= dmni_mem_address(29 downto 0);
        end if;
    end process;

    sequential_attr: process(clock, reset)
    begin            
        if reset = '1' then
            cpu_mem_data_address_reg <= ZERO;
            cpu_mem_instr_address_reg <= ZERO;
            cpu_mem_data_write_reg <= ZERO;
            cpu_mem_data_write_byte_enable_reg <= ZERO(3 downto 0);
            irq_mask_reg <= ZERO(7 downto 0);
            time_slice <= ZERO;
            tick_counter <= ZERO;
            pending_service <= '0';
            ack_app <= '0';
            slack_update_timer <= ZERO;
            next_page_reg <= x"00000000";
            current_page <= x"00";
            change_page <= '0';
        elsif (clock'event and clock = '1') then
            if cpu_mem_pause = '0' then
                -- reading_data_reg <= reading_data;
                cpu_mem_data_address_reg <= cpu_mem_data_address;
                cpu_mem_instr_address_reg <= cpu_mem_instr_address;
                cpu_mem_data_write_reg <= cpu_mem_data_write;
                cpu_mem_data_write_byte_enable_reg <= cpu_mem_data_write_byte_enable;

                -- modified: pe controls current page (not at CPU anymore)

                if irq_ret = '1' or ecall = '1' then                
                    current_page <= "00000000";
                end if;

                -- Clarvi mod: get NEXT_PAGE and store it in a register
                if cpu_mem_data_address_reg = NEXT_PAGE and write_enable = '1' then
                    next_page_reg <= cpu_mem_data_write_reg;
                end if;

                -- Clarvi mod: whem mret is executed, set change_page flag
                if cpu_mem_instr_read = x"30200073" then
                    change_page <= '1';
                end if;

                -- When change_page flag is set, change current page
                if change_page = '1' then
                    change_page <= '0';
                    current_page <= next_page_reg(PAGE_SIZE_H_INDEX+8 downto PAGE_SIZE_H_INDEX+1);
                end if;

                if cpu_mem_data_address_reg = IRQ_MASK and write_enable = '1' then
                    irq_mask_reg <= cpu_mem_data_write_reg(7 downto 0);
                end if;     
               -- Decrements the time slice when executing a task (current_page /= x"00") or handling a syscall (syscall = '1')
                if time_slice > 1 then
                    time_slice <= time_slice - 1;
                end if;  

                if(cpu_mem_data_address_reg = PENDING_SERVICE_INTR and write_enable = '1') then
                    if cpu_mem_data_write_reg = ZERO then
                        pending_service <= '0';
                    else
                        pending_service <= '1';
                    end if;
                end if; 
            end if;
            
            if cpu_mem_data_address_reg = SLACK_TIME_MONITOR and write_enable = '1' then
            	slack_update_timer <= cpu_mem_data_write_reg;
            elsif slack_update_timer < SLACK_MONITOR_WINDOW then
            	slack_update_timer <= slack_update_timer + 1;
            end if;
                                    
            if cpu_mem_data_address_reg = TIME_SLICE_ADDR and write_enable = '1' then
                time_slice <= cpu_mem_data_write_reg;
            end if;
                
            -- modified: Clarvi
            if cpu_mem_data_address_reg = ACK_APP_REG and invalidate_ack = '0' then
                ack_app <= '1';
            end if;
            if req_app(31) = '0' then 
                ack_app <= '0';
                invalidate_ack <= '1';
            end if;

            -- Asserts that: if invalidate flag is set when it shouldn't, it will come to the right state...
            if cpu_mem_data_address_reg /= ACK_APP_REG then
                invalidate_ack <= '0';
            end if;

            tick_counter <= tick_counter + 1;   
        end if;
    end process sequential_attr;

    -- Not used with this version (Clarvi does not support it)...
    -- clock_stop: process(reset,clock)
    -- begin
    --     if(reset = '1') then
    --         tick_counter_local <= (others=> '0');
    --         clock_aux <= '1';
    --     else
    --         if cpu_mem_data_address_reg = CLOCK_HOLD and write_enable = '1' then
    --             clock_aux <= '0';
    --         --elsif tx_router(LOCAL) = '1' or ni_intr = '1' or time_slice = x"00000001" then 
    --         elsif ni_intr = '1' or time_slice = x"00000001" or irq_status(1) = '1' then 
    --             clock_aux <= '1';
    --         end if;

    --         if(clock_aux ='1' and clock ='1') then
    --             clock_hold_s <= '1';
    --             tick_counter_local <= tick_counter_local + 1;
    --         else
    --             clock_hold_s <= '0';
    --         end if;
    --     end if;
    -- end process clock_stop;
end architecture structural;
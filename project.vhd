library IEEE;
use IEEE.STD_LOGIC_1164.ALL;            --importo le librerie che mi permettono di usare tipi come
use IEEE.STD_LOGIC_UNSIGNED.ALL;        -- std_logic e std_logic_vector e di eseguire operazioni come + e -
use IEEE.NUMERIC_STD.ALL;

entity project_reti_logiche is          --definizione entità del componente da progettare
  port (                                --questa sezione è copia e incollata dalla specifica
    i_clk   : in std_logic;
    i_rst   : in std_logic;
    i_start : in std_logic;
    i_add   : in std_logic_vector(15 downto 0);
    i_k     : in std_logic_vector(9 downto 0);
    
    o_done  : out std_logic;
    
    o_mem_addr : out std_logic_vector(15 downto 0);
    i_mem_data : in std_logic_vector(7 downto 0);
    o_mem_data : out std_logic_vector(7 downto 0);
    o_mem_we   : out std_logic;
    o_mem_en   : out std_logic
  );
end project_reti_logiche;

architecture project_reti_logiche_arch of project_reti_logiche is
------------------------------------------------------------------------------------- definizione dei sottocomponenti
component address_adder is                              --definisco segnali input/output del componente address_adder
    port (                                              --per documentazione vedi la port all'interno della
       addr_in : in std_logic_vector(15 downto 0);      --entity del componente
       a_rst   : in std_logic;
       a_clk   : in std_logic;
       a_rst_comp   : in std_logic;
        
       en_addr_count : in std_logic;
       en_addr_add   : in std_logic;
        
       a_o_mem_addr  : out std_logic_vector(15 downto 0) 
    );
end component address_adder;

component word_counter is                               --definisco segnali input/output del componente word_counter
    port (                                              --per documentazione vedi la port all'interno della
        w_k      : in std_logic_vector(9 downto 0);     --entity del componente
        w_rst    : in std_logic;
        w_clk    : in std_logic;
        w_rst_comp    : in std_logic;
        
        en_word_count : in std_logic;
        
        o_continue    : out std_logic    
    );
end component word_counter;

component last_data_not_zero is                         --definisco segnali input/output di last_data_not_zero
    port (                                              --per documentazione vedi la port all'interno della
        ld_clk        : in std_logic;                   --entity del componente
        ld_rst        : in std_logic;
        ld_rst_comp   : in std_logic;
        ld_i_mem_data : in std_logic_vector (7 downto 0);
        
        en_last       : in std_logic;
        
        o_reset_to31  : out std_logic;
        ld_o_mem_data : out std_logic_vector (7 downto 0)
    );
end component last_data_not_zero;

component credibility_counter is                        --definisco segnali input/output del credibility_counter
    port (                                              --per documentazione vedi la port all'interno della
        cr_clk        : in std_logic;                   --entity del componente
        cr_rst        : in std_logic;
        cr_rst_comp   : in std_logic;
        in_reset_to31 : in std_logic;
        
        en_cred       : in std_logic;
        
        cr_o_mem_data : out std_logic_vector (7 downto 0)
    );
end component credibility_counter;

component mux_wr is                                         --definisco segnali input/output del componente mux_wr
    port (                                                  --per documentazione vedi la port all'interno della entity
        m_ld_o_mem_data : in std_logic_vector (7 downto 0); --del componente
        m_cr_o_mem_data : in std_logic_vector (7 downto 0);
        
        sel_out         : in std_logic;
        
        m_o_mem_data    : out std_logic_vector (7 downto 0)
    );
end component mux_wr;

component fsm is                                        --definisco segnali input/output del componente fsm
    port (                                              --per documentazione vedi la port all'interno della entity
        fsm_clk        : in std_logic;                  --del componente
        fsm_rst        : in std_logic;
        fsm_start      : in std_logic;
        
        w_continue     : in std_logic;
        fsm_reset_to31 : in std_logic;
        
        fsm_en_word_count : out std_logic;
        
        fsm_en_addr_add   : out std_logic;
        fsm_en_addr_count : out std_logic;
        
        fsm_en_ram     : out std_logic;
        fsm_en_w       : out std_logic;
        
        fsm_sel_out    : out std_logic;
        
        fsm_en_cred    : out std_logic;
        
        fsm_en_last    : out std_logic;
        
        fsm_rst_comp   : out std_logic;
        
        fsm_o_done     : out std_logic
    );
end component fsm;

---------------------------------------------------------- definisco segnali ausiliari che collegano i sottocomponenti

signal sign_en_addr_count : std_logic;                  --segnali per collegare address_adder a fsm
signal sign_en_addr_add   : std_logic;
signal sign_rst_comp : std_logic;

signal sign_en_wcount     : std_logic;                  --segnali per collegare word_counter a fsm
signal sign_continue      : std_logic;

signal sign_en_last       : std_logic;                  --segnali per collegare last_data_not_zero a fsm
signal sign_reset_31      : std_logic;
                                                        --segnale per collegare last_data_not_zero a mux_wr
signal sign_ld_o_mem_data : std_logic_vector (7 downto 0);  

signal sign_en_cred       : std_logic;                  --segnale per collegare credibility_counter a fsm
                                                        --segnale per collegare credibility_counter a mux_wr
signal sign_cr_o_mem_data : std_logic_vector (7 downto 0);

signal sign_sel_out       : std_logic;                  --segnale per collegare mux_wr a fsm

begin                           --descrivo l'architettura del componente project_reti_logiche
------------------ effettuo i collegamenti tra i componenti tramite i segnali di ingresso e ausiliari
    aadd : address_adder port map (
        addr_in => i_add,                       --collego i segnali interni a input esterni
        a_rst   => i_rst,
        a_clk   => i_clk,
        a_rst_comp => sign_rst_comp,
                                       
        en_addr_count => sign_en_addr_count,    --collego i segnali interni alla fsm
        en_addr_add   => sign_en_addr_add,
              
        a_o_mem_addr => o_mem_addr              --collego il segnale interno all'output
    );
    
    wcount : word_counter port map (
        w_k     => i_k,                         --collego i segnali interni a input esterni
        w_rst   => i_rst,
        w_clk   => i_clk,
        w_rst_comp => sign_rst_comp,
        
        en_word_count => sign_en_wcount,        --collego i segnali interni alla fsm
        
        o_continue => sign_continue
    );
    
    reg_last_data : last_data_not_zero port map (
        ld_clk        => i_clk,                 --collego i segnali interni a input esterni
        ld_rst        => i_rst,
        ld_rst_comp   => sign_rst_comp,         --collego il segnale interni alla fsm
        ld_i_mem_data => i_mem_data,
        
        en_last => sign_en_last,                --collego i segnali interni alla fsm
        
        o_reset_to31  => sign_reset_31,
        ld_o_mem_data => sign_ld_o_mem_data     --collego il segnale interno al mux_wr
    );
    
    cred_count : credibility_counter port map (
        cr_clk        => i_clk,                 --collego i segnali interni a input esterni
        cr_rst        => i_rst,
        cr_rst_comp   => sign_rst_comp,         --collego il segnale interni alla fsm
        in_reset_to31 => sign_reset_31,
        
        en_cred => sign_en_cred,
        
        cr_o_mem_data => sign_cr_o_mem_data     --collego il segnale interno al mux_wr
    );
    
    multipl : mux_wr port map (
        m_ld_o_mem_data => sign_ld_o_mem_data,  --collego il segnale interno al last_data_not_zero
        m_cr_o_mem_data => sign_cr_o_mem_data,  --collego il segnale interno al credibility_counter
        
        sel_out => sign_sel_out,                --collego il segnale interno alla fsm
        
        m_o_mem_data => o_mem_data              --collego il segnale interno all'output
    );
    
    finite_state_machine : fsm port map (
        fsm_clk   => i_clk,                     --collego i segnali interni a input esterni
        fsm_rst   => i_rst,
        fsm_start => i_start,
        
        w_continue     => sign_continue,        --collego i segnali interni ai componenti
        fsm_reset_to31 => sign_reset_31,
        
        fsm_en_word_count => sign_en_wcount,
        
        fsm_en_addr_add   => sign_en_addr_add,
        fsm_en_addr_count => sign_en_addr_count,
        
        fsm_en_ram => o_mem_en,
        fsm_en_w   => o_mem_we,
        
        fsm_sel_out => sign_sel_out,
        
        fsm_en_cred => sign_en_cred,
        
        fsm_en_last => sign_en_last,
        
        fsm_rst_comp => sign_rst_comp,
        
        fsm_o_done  => o_done                   --collego i segnali interni all'output
    );

end project_reti_logiche_arch;

--------------------------------------------------------------------------------------------------------
--Definisco l'entità address_adder (che sposta l'indirizzo da cui leggere o scrivere in memoria aumentandolo)
--e la sua architettura
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;                --vanno ridichiarati per ogni entità
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity address_adder is 
   port (
       addr_in : in std_logic_vector(15 downto 0);      -- corrisponde all'ingrsso ADD (16 bit)
       a_rst   : in std_logic;                          -- connesso al rst del componente
       a_clk   : in std_logic;                          -- connesso al clk del componente
       a_rst_comp : in std_logic;                       -- connesso al segnale inviato dalla fsm, resetta il componente
        
       en_addr_count : in std_logic;                    --connessi ai segnali inviati dalla fsm, il primo permette l'
       en_addr_add   : in std_logic;                    --incremento del counter che corrisponde all'offset, l'altro
                                                        --permette all'adder di eseguire la somma
       a_o_mem_addr  : out std_logic_vector(15 downto 0)    -- connesso all'uscita verso la RAM
   );
end entity address_adder;
 
architecture address_adder_arch of address_adder is
    -- segnali interni al componente che hanno funzione di supporto
signal stored_value : std_logic_vector (15 downto 0);   --rappresenta il valore della somma tra ADD e offset
signal val_count    : std_logic_vector (15 downto 0);   --rappresenta il valore dell'offset
 
begin
    a_o_mem_addr <= stored_value;                       --mette in uscita il dato salvato in stored_value

    check_for_update : process(a_clk, a_rst, a_rst_comp)  --processo che si sveglia ricevo uno dei rst
    begin                      --oppure a ogni colpo di clk controlla se va aggiornato uno dei valori
        if a_rst = '1' or a_rst_comp = '1' then         -- resetta il componente
            stored_value <= (others => '0');            --inizializza valore in uscita a 0
            val_count <= (others => '0');               --inizializza contatore a 0
        elsif a_clk'event and a_clk = '1' then          --se siamo su un fronte di salita del clock
            if en_addr_count = '1' then                 --se l'enable è alto incrementa il contatore
                val_count <= val_count + 1;             --non sommo un byte ma basta fare +1
            elsif en_addr_add = '1' then                --se l'enable è alto esegue la somma
                stored_value <= addr_in + val_count;
            end if;
        end if;
    end process;
 
end address_adder_arch;
 
--------------------------------------------------------------------------------------------------------
--Definisco l'entità word_counter (che serve per contare quante parole mancano al termine della
--computazione) e la sua architettura
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;
 
entity word_counter is
    port (
        w_k   : in std_logic_vector(9 downto 0);    -- corrisponde all'ingrsso K (10 bit)
        w_rst : in std_logic;                       -- connesso al rst del componente
        w_clk : in std_logic;                       -- connesso al clk del componente
        w_rst_comp : in std_logic;                  -- connesso al segnale inviato dalla fsm, resetta il componente
        
        en_word_count: in std_logic;                -- connesso al segnale inviato dalla fsm, permette di
                                                    -- incrementare il counter
        o_continue : out std_logic                  --connesso al segnale inviato alla fsm, indica se si deve continuare
    );                                              -- l'elaborazione o se si deve terminare
end word_counter;
 
architecture word_counter_arch of word_counter is
       -- segnali di supporto
signal val_wcount : std_logic_vector (9 downto 0);  -- rappresenta il valore attuale del contatore
signal w_to_encode : std_logic_vector (9 downto 0); -- rappresenta il valore da codificare
 
begin                                               --parte combinatoria asincrona:
    w_to_encode <= w_k xor val_wcount;              --calcolo il valore da codificare con XOR tra ingresso e contatore
    
    with w_to_encode select                         -- codifico il segnale interpretando il risultato dello xor
    o_continue <= '0' when "0000000000",            -- se ho 0: i due valori sono uguali quindi il componente si
                  '1' when others;                  -- deve fermare, per qualsiasi altro valore posso continuare
    
    increment_counter: process (w_clk, w_rst, w_rst_comp)   -- processo sequenziale sincrono:
    begin      -- si sveglia se riceve rst oppure a ogni colpo di clk aggiorna il valore 
        if w_rst = '1' or w_rst_comp = '1' then             -- resetta il componente
            val_wcount <= (others => '0');
        elsif w_clk'event and w_clk = '1' then              -- fronte di salita del clk
            if en_word_count = '1' then                     -- se l'enable è alto incrementa il contatore
                val_wcount <= val_wcount + 1;
            end if;
        end if;
    end process;
    
end word_counter_arch;

--------------------------------------------------------------------------------------------------------
--Definisco l'entità last_data_not_zero (che serve a verificare se il dato letto dalla memoria è zero e
--se non lo è a memorizzarlo) e la sua architettura
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity last_data_not_zero is
    port (
        ld_clk : in std_logic;              -- connesso al clk del componente
        ld_rst : in std_logic;              -- connesso al rst del componente
        ld_rst_comp : in std_logic;         -- connesso al segnale inviato dalla fsm, resetta il componente
        ld_i_mem_data : in std_logic_vector (7 downto 0);    -- connesso all'ingresso dalla la RAM
        
        en_last : in std_logic;             --connesso al segnale inviato dalla fsm, permette la valutazione del
                                            --dato letto
        o_reset_to31  : out std_logic;      --segnale inviato alla fsm, indica se va resettata la credibilità
        ld_o_mem_data : out std_logic_vector (7 downto 0) -- connesso al mux_wr, indica cosa deve essere
    );                                                    --scritto nella RAM
end last_data_not_zero;
 
architecture last_data_not_zero_arch of last_data_not_zero is
       -- segnali di supporto
signal ld_to_encode_sig : std_logic_vector (7 downto 0);    --segnale da codificare
signal stored_value_sig : std_logic_vector (7 downto 0);    --segnale con l'ultimo dato non 0 salvato
signal reset31_sig : std_logic;                     --segnale da inviare che resetta la credibility
 
begin                     -- parte asincrona per verificare che il dato letto non sia 0
    ld_to_encode_sig <= ld_i_mem_data xor "00000000";   -- lo XOR verifica se sono uguali
    
    with ld_to_encode_sig select                    -- nel caso siano uguali non devo resettare a 31 la credibility
        reset31_sig <= '0' when "00000000",
        '1' when others;                            -- se invece sono diversi alzo il segnale di reset a 31
    
    ld_o_mem_data <= stored_value_sig;              -- mette in uscita il valore salvato nel registro(l'ultimo!=0)
    
    activate_sig_rst_31: process(ld_clk)            -- processo sequenziale sincrono:
    begin                               -- si sveglia a ogni colpo di clk e controlla se aggiornare il valore 
        if ld_clk'event and ld_clk = '0' then
            if en_last = '1' then                   -- se l'enable è alto collega il segnale ottenuto dallo
                o_reset_to31 <= reset31_sig;        -- XOR all'uscita che resetta a 31 la credibility
            end if;
        end if;
    end process;
    
    register_data: process(ld_clk, ld_rst, ld_rst_comp) -- processo sincrono:
    begin                -- si sveglia se riceve rst oppure a ogni colpo di clk controlla se aggiornare il valore 
        if ld_rst = '1' or ld_rst_comp = '1' then       -- resetto il componente
            stored_value_sig <= (others => '0');
        elsif ld_clk'event and ld_clk = '1' then        -- fronte di salita del clk
            if en_last = '1' then                       -- se l'enable è alto e se il segnale reset a 31
                if reset31_sig = '1' then               -- è alto allora salvo il valore letto
                    stored_value_sig <= ld_i_mem_data;
                end if;
            end if;
        end if;
    end process;
 
end last_data_not_zero_arch;

--------------------------------------------------------------------------------------------------------
--Definisco l'entità credibility_counter (che serve a calcolare il valore di credibilita del dato letto 
--dalla memoria) e la sua architettura
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity credibility_counter is
    port (
        cr_clk : in std_logic;              -- connesso al clk del componente
        cr_rst : in std_logic;              -- connesso al clk del componente
        cr_rst_comp : in std_logic;         -- connesso al segnale inviato dalla fsm, resetta il componente
        in_reset_to31 : in std_logic;       -- connesso al segnale inviato dal last_data_not_zero, serve
                                            -- a resettare la credibility a 31 
        en_cred : in std_logic;             --connesso al segnale inviato dalla fsm, permette la valutazione della
                                            --credibility
        cr_o_mem_data : out std_logic_vector (7 downto 0) -- connesso al mux_wr, indica cosa deve essere
    );                                                    --scritto nella RAM
end credibility_counter;
 
architecture credibility_counter_arch of credibility_counter is
       -- segnali di supporto
signal val_cr_count : std_logic_vector (4 downto 0); -- contiene il valore della credibility

begin
    cr_o_mem_data <= "000" & val_cr_count;          -- parte asincrona che concatena e mette in uscita
                                                    -- il valore di credibilità
    calculate_credibility: process(cr_clk, cr_rst, cr_rst_comp)   -- processo sincrono
    begin                               -- si sveglia se riceve rst oppure a ogni colpo di clk aggiorna il valore
        if cr_rst = '1' or cr_rst_comp = '1' then   --resetta il componente
            val_cr_count <= (others => '0');
        elsif cr_clk'event and cr_clk = '1' then    -- fronte di salita del clk
            if en_cred = '1' then                   -- se l'enable è alto e se il segnale di reset a 31
                if in_reset_to31 = '1' then         -- è alto, riporta il contatore a 31
                    val_cr_count <= "11111";
                elsif in_reset_to31 = '0' then      -- se il segnale di reset a 31 è basso
                    if val_cr_count > 0 then        -- se la credibility è maggiore di 0, decrementa il contatore
                        val_cr_count <= val_cr_count - 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

end credibility_counter_arch;

--------------------------------------------------------------------------------------------------------
--Definisco l'entità mux_wr (che serve a stabilire da dove è preso il dato da scrivere in memoria:
--dal credibility_counter o dal registro last_data_not_zero) e la sua architettura
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mux_wr is
    port (
        m_ld_o_mem_data : in std_logic_vector (7 downto 0);     --connesso al last_data_not_zero
        m_cr_o_mem_data : in std_logic_vector (7 downto 0);     --connesso al credibility_counter
        
        sel_out : in std_logic;         -- connesso al segnale inviato dalla fsm, indica quale dato scrivere
        
        m_o_mem_data : out std_logic_vector (7 downto 0)        -- connesso all'uscita verso la RAM
    );
end mux_wr;
 
architecture mux_wr_arch of mux_wr is
 
begin     -- processo asincrono che mette in uscita, a seconda di sel_out, il valore != zero da scrivere
    m_o_mem_data <= m_ld_o_mem_data when sel_out = '0' else         -- nel caso l'ultimo valore letto è 0,
                    m_cr_o_mem_data when sel_out = '1';             -- oppure il val di credibility

end mux_wr_arch;

--------------------------------------------------------------------------------------------------------
--Definisco l'entità fsm (che serve a gestire l'avanzamento degli stati e i segnali tra i vari
--componenti) e la sua architettura
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity fsm is
    port (
        fsm_clk   : in std_logic;           -- connesso al clk del componente
        fsm_rst   : in std_logic;           -- connesso al rst del componente
        fsm_start : in std_logic;           -- connesso allo start del componente
        
        w_continue : in std_logic;          -- connesso al segnale inviato al word_counter
        fsm_reset_to31 : in std_logic;      -- connesso al segnale inviato al credibility_counter
        
        fsm_en_word_count : out std_logic;  -- connesso al segnale inviato al word_counter
        
        fsm_en_addr_add   : out std_logic;  -- connessi ai segnali inviati all'address_adder
        fsm_en_addr_count : out std_logic;
        
        fsm_en_ram : out std_logic;         -- connessi ai segnali inviati alla RAM
        fsm_en_w   : out std_logic;
        
        fsm_sel_out : out std_logic;        -- connesso al segnale inviato al mux_wr
        
        fsm_en_cred : out std_logic;        -- connesso al segnale inviato al credibility_counter
        
        fsm_en_last : out std_logic;        -- connesso al segnale inviato al last_data_not_zero
        
        fsm_rst_comp : out std_logic;       -- connesso al segnale inviato ai componenti per resettarli
        
        fsm_o_done  : out std_logic         -- connesso al segnale di uscita del componente
    );
end fsm;
 
architecture fsm_arch of fsm is
            -- tutti gli stati possobili
type S is (IDLE, CHECK_WCOUNT, INCR_WCOUNT, READ_RAM, CHECK_DATA_NOT0, WRITE_RAM_DATA, INCR_ADDR_D, 
            CALC_CRED, WRITE_RAM_CRED, INCR_ADDR_CR, RESET_COMPONENTS, FINAL);
            
signal curr_state : S;                      -- segnale che definisce lo stato corrente
 
begin   
    change_state : process (fsm_clk, fsm_rst)  -- processo sincrono che permette il cambiamento di stato
    begin
        if fsm_rst = '1' then                  -- reset riporta allo stato di IDLE
            curr_state <= IDLE;
        elsif fsm_clk'event and fsm_clk = '1' then  -- fronte di salita del clk
            case curr_state is
                when IDLE =>                   -- se fsm è in IDLE e riceve start passa a CHECK_WCOUNT
                    if fsm_start = '1' then
                        curr_state <= CHECK_WCOUNT;
                    end if;
                    
                when CHECK_WCOUNT =>           -- se fsm è in CHECK_WCOUNT e continue è alto passa a INCR_WCOUNT
                    if w_continue = '1' then
                        curr_state <= INCR_WCOUNT;
                    elsif w_continue = '0' then         -- altrimenti se è basso passa a RESET_COMPONENTS
                        curr_state <= RESET_COMPONENTS;
                    end if;
                    
                when INCR_WCOUNT =>           -- se fsm è in INCR_WCOUNT passa a READ_RAM
                    curr_state <= READ_RAM;
                    
                when READ_RAM =>              -- se fsm è in READ_RAM passa a CHECK_DATA_NOT0
                    curr_state <= CHECK_DATA_NOT0;
                    
                when CHECK_DATA_NOT0 =>       -- se fsm è in CHECK_DATA_NOT0
                    if fsm_reset_to31 = '1' then        -- e rst a 31 è alto passa a INCR_ADDR_D
                        curr_state <= INCR_ADDR_D;
                    elsif fsm_reset_to31 = '0' then     -- altrimenti se è basso passa a WRITE_RAM_DATA
                        curr_state <= WRITE_RAM_DATA;
                    end if;
                    
                when WRITE_RAM_DATA =>        -- se fsm è in WRITE_RAM_DATA passa a INCR_ADDR_D
                    curr_state <= INCR_ADDR_D;
                
                when INCR_ADDR_D =>           -- se fsm è in INCR_ADDR_D passa a CALC_CRED
                    curr_state <= CALC_CRED;
                    
                when CALC_CRED =>             -- se fsm è in CALC_CRED passa a WRITE_RAM_CRED
                    curr_state <= WRITE_RAM_CRED;
                    
                when WRITE_RAM_CRED =>        -- se fsm è in WRITE_RAM_CRED passa a INCR_ADDR_CR
                    curr_state <= INCR_ADDR_CR;
                    
                when INCR_ADDR_CR =>          -- se fsm è in INCR_ADDR_CR passa a CHECK_WCOUNT
                    curr_state <= CHECK_WCOUNT;
                    
                when RESET_COMPONENTS =>      -- se fsm è in RESET_COMPONENTS e start è basso passa a FINAL
                    curr_state <= FINAL;
                    
                when FINAL =>                 -- se fsm è in FINAL e start è basso passa a IDLE
                    if fsm_start = '0' then
                        curr_state <= IDLE;
                    end if;
            end case;
        end if;
    end process;
    
    set_signals : process (curr_state)      -- processo asincrono (invocato al variare dello stato)
    begin                                   -- che assegna valore a tutti i segnali
            fsm_en_word_count <= '0';
            fsm_en_addr_add <= '0';
            fsm_en_addr_count <= '0';       -- ogni volta che il processo è risvegliato inizializza
            fsm_en_ram <= '0';              -- tutti i segnali
            fsm_en_w <= '0';
            fsm_sel_out <= '0';
            fsm_en_cred <= '0';
            fsm_en_last <= '0';
            fsm_o_done <= '0';
            fsm_rst_comp <= '0';
            
        if curr_state = IDLE then
            fsm_en_addr_add <= '1';         -- sommo prima di partire per ottenere il primo indirizzo in uscita
            
        elsif curr_state = INCR_WCOUNT then
            fsm_en_word_count <= '1';       -- attiva il word_counter
    
        elsif curr_state = READ_RAM then
            fsm_en_ram <= '1';              -- attiva la lettura da RAM
            fsm_en_addr_count <= '1';       -- incrementa contatore indirizzo per far si che l'address_
                                            -- adder sia pronto per la add
        elsif curr_state = CHECK_DATA_NOT0 then
            fsm_en_last <= '1';             -- attiva il last_data_not_zero
            
        elsif curr_state = WRITE_RAM_DATA then
            fsm_en_ram <= '1';              -- attiva la scrittura in memoria
            fsm_en_w <= '1';
            fsm_sel_out <= '0';             -- garantisce che sia scritto il dato != 0 dal registro
        
        elsif curr_state = INCR_ADDR_D then
            fsm_en_addr_add <= '1';         -- attiva l'address_adder eseguendo la somma
        
        elsif curr_state = CALC_CRED then
            fsm_en_cred <= '1';             -- attiva il credibility_counter
            fsm_en_addr_count <= '1';       -- incrementa contatore indirizzo per far si che l'address_
                                            -- adder sia pronto per la add
        elsif curr_state = WRITE_RAM_CRED then
            fsm_en_ram <= '1';              -- attiva la scrittura in memoria
            fsm_en_w <= '1';
            fsm_sel_out <= '1';             -- garantisce che sia scritta la credibility
            
        elsif curr_state = INCR_ADDR_CR then
            fsm_en_addr_add <= '1';         -- attiva l'address_adder eseguendo la somma
            
        elsif curr_state = RESET_COMPONENTS then
            fsm_rst_comp <= '1';            -- resetta i componenti in vista di una nuova elaborazione
            
        elsif curr_state = FINAL then
            fsm_o_done <= '1';              -- alza il segnale di done in uscita
            
        end if;
    end process;
 
end fsm_arch;

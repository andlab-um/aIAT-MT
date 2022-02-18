Agents=59;
trials=200;
iters=Agents*trials;

for a       = 1 : Agents
    
    % First, simulate RTs and pop for the congruent block
    conRT   = nan(trials*2,1);
    for s   = 1 : length(stim)
        [conRT((s-1)*trials+1:s*trials)] = DriftDiffusionFunction(Con(a,s),trials);
    end
    allRTs{a,1}         = conRT;
    
    
    % Then, simulate RTs for the incongruent block
    incRT   = nan(trials*2,1);
    for s   = 1 : length(stim)
        [incRT((s-1)*trials+1:s*trials)] = DriftDiffusionFunction(Inc(a,s),trials);
    end
    allRTs{a,2}         = incRT;
    
    % Finally, compute the IAT from the mean reaction times in each block
    RTs(a,1)      	= nanmean(conRT);
    RTs(a,2)    	= nanmean(incRT);
    RTs(a,3) = nanmean(conRT(1:trials));%con:ev1
    RTs(a,4) = nanmean(conRT(trials+1:2*trials));%con:ev2
    RTs(a,5) = nanmean(incRT(1:trials));%incon:ev1
    RTs(a,6) = nanmean(incRT(trials+1:2*trials));%incon:ev2
    IAT(a,1)            = (nanmean(incRT) - nanmean(conRT)) / nanstd([incRT ; conRT]); 
    IAT(a,2)            = (RTs(a,3) - RTs(a,5)) / nanstd([conRT(1:trials) ; incRT(1:trials)]);
    IAT(a,3)            = (RTs(a,4) - RTs(a,6)) / nanstd([conRT(trials+1:2*trials) ; incRT(trials+1:2*trials)]);
    %clear incRT conRT
    
    
end



function[RT] = DriftDiffusionFunction(coh,iters)

%  Inputs : coh             Strength of inputs (equivalent to moving dot coherence, vary between -1 and 1)
%           iters           Number of iterations of the mean-field model to simulate

%  Outputs: RT              Reaction time (s)


%% First, initialise the simulation parameters
%  Firing rate parameters
a       = 270;                      % Gain factor (Hz / nA)
b       = 108;                      % Hz
d       = 0.154;                    % s

%  NMDA conductance parameters
Tau_s   = 60;                       % Time constant (ms)
gamma_s = 0.641;                    % dimensionless

%  Synaptic interaction parameters
Jll     = 0.3725;                   % Recurrent excitation strength in left population (nA)
Jrr     = Jll;                      % Recurrent excitation strength in right population (nA)
Jlr     = 0.1137;                   % Inhibitory coupling strength between left and right populations (nA)
Jrl     = Jlr;                      % Inhibitory coupling strength between right and left populations (nA)

%  Noise parameters
Io      = 0.3297;                   % Mean noise input (nA)
So      = 0.009;                    % Standard deviation of noise (nA)
To      = 2;                        % Time constant (ms)

%  Input parameters
Jext    = 1.1e-3;                   % Synaptic strength of external input (nA/Hz)
mu_o    = 30;                       % Firing rate for zero coherence (Hz)
f       = 0.45;                     % Firing rate coherence gain (dimensionless)
targOn  = 1000;                     % Time at which to present targets (ms)
stimOn  = 3000;                     % Time at which to present stimulus (ms)
Tau_ad  = 40;                       % Time constant of adaptation (ms)

%  Simulation parameters
dt      = 0.5;                      % Time step (ms)
t_max   = 5000;                     % Simulation length (s)
thresh  = 55;                       % Threshold for response to be made (Hz)


%% Assign memory for the dynamics and output
sL      = 0.1*ones(iters,1);        % NMDA gating variable for left population
sR      = 0.1*ones(iters,1);        % NMDA gating variable for right population
Il      = zeros(iters,1);           % Synaptic currents to left population (nA)
Ir      = zeros(iters,1);           % Synaptic currents to right population (nA)
Iol     = zeros(iters,1);           % Noise input to left population (nA)
Ior     = zeros(iters,1);           % Noise input to right population (nA)
Iin_l   = 0;                        % External input to left population (nA)
Iin_r   = 0;                        % External input to right population (nA)
Itarg   = 0;                        % Input from visual target (nA)Ifix
tvec    = dt:dt:t_max;              % Time base (ms)
rates   = nan(iters,t_max/dt,2);    % Memory for the firing rates of each population (Hz)


%% Run the dynamics
for t   = 1 : length(tvec)
    
    % Compute the input from the visual target and visual stimuli (moving dots)
    Itarg   = (tvec(t)>=targOn && tvec(t)<stimOn) * Jext * (50 + 100 * exp(-(tvec(t)-targOn)/Tau_ad));
    Itarg   = Itarg + (tvec(t) >= stimOn) * Jext * (6 + 44 * exp(-(tvec(t)-stimOn)/Tau_ad));
    Iin_l   = (tvec(t) >= (stimOn+100)) * Jext * mu_o * (1 - f*coh);
    Iin_r   = (tvec(t) >= (stimOn+100)) * Jext * mu_o * (1 + f*coh);
    
    % Compute the noise input to each population
    Iol     = Iol + ( -(Iol - Io) + wgn(iters,1,1)*sqrt(To)*So) * (dt / To);
    Ior     = Ior + ( -(Ior - Io) + wgn(iters,1,1)*sqrt(To)*So) * (dt / To);
    
    % Compute the total synaptic current input to each population
    Il      = Jll*sL - Jlr*sR + Iin_l + Itarg + Iol;
    Ir      = Jrr*sR - Jrl*sL + Iin_r + Itarg + Ior;
    
    % Compute the firing rates of each population (constrain to be
    % non-negative)
    rl      = (a.*Il -b) ./ (1 - exp(-d.*(a.*Il -b)));
    left=rl;
    rr      = (a.*Ir -b) ./ (1 - exp(-d.*(a.*Ir -b)));
    
    rl(rl<0)    = 0;  rr(rr<0)    = 0;
    
    % Compute the NMDA gating variable for each population
    sL      = sL + (-sL/Tau_s + (1-sL)*gamma_s.*rl/1000) * dt;
    sR      = sR + (-sR/Tau_s + (1-sR)*gamma_s.*rr/1000) * dt;
    
    % Log the firing rates
    rates(:,t,:)    = cat(3,rl,rr);
    
end
clear t


%% Identify the reaction time and plot the output
RT          = (rates>=thresh).*repmat(tvec,[iters 1 2]);
RT(RT==0)   = nan;
RT          = squeeze(min(RT,[],2));
RT          = (nanmean(RT,2)-stimOn)/1000;  % Reaction time (s)


end

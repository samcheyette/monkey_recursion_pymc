
import pymc3 as pm

#import thano.compile.ops as as_op
from helpers import *
import matplotlib.pyplot as plt
import seaborn as sns
from pymc3.backends.base import merge_traces

#np.random.seed(123)


def model():
    global data
    alpha_prior = 1.
    beta_prior = 0.1
    alpha_init = np.ones((N_GROUPS,1))
    noise_init = np.ones((N_GROUPS,1))*1e-2

    parts_ones = np.ones((TOTAL_PARTS))
    data_ones = np.ones(len(data[0]))

    hds = store_hds(paren_lst, filt)
    ns = np.sum(data, axis=1)


    m_ass = np.where(assignments == 0)
    k_ass = np.where(assignments == 1)
    t_ass = np.where(assignments==2)

    n_monk = len(m_ass[0])
    n_kid = len(k_ass[0])
    n_tsim = len(t_ass[0])



    with pm.Model() as m:
        alpha = pm.Exponential('alpha', alpha_prior,
                 shape=(N_GROUPS,1))


        #alpha = np.ones((N_GROUPS, 1)) * 20.
        beta = pm.Dirichlet('beta', np.ones(N_ALGS)*beta_prior,
                        # testval=np.ones(N_ALGS),
                            shape=(N_GROUPS,N_ALGS))



        theta = pm.Dirichlet('theta',  alpha[assignments] * beta[assignments], 
                            shape=(TOTAL_PARTS,N_ALGS)) 




        noise = pm.Beta("noise", 1,9, shape=3, testval=0.1)
        theta_resp = theta.dot(algorithms) 
        """
        monkey_theta = theta[m_ass]
        kid_theta = theta[k_ass]
        tsim_theta = theta[t_ass]


        new_algs_monkey = format_algs_theano(hds, noise[0])
        new_algs_kid = format_algs_theano(hds, noise[1])
        new_algs_tsim = format_algs_theano(hds, noise[2])


        monkey_algs = monkey_theta.dot(new_algs_monkey)
        kid_algs = kid_theta.dot(new_algs_kid)
        tsim_algs = tsim_theta.dot(new_algs_tsim)


        lst = []
        for i in xrange(n_monk):
            lst.append(monkey_algs[i])
        for i in xrange(n_kid):
            lst.append(kid_algs[i])
        for i in xrange(n_tsim):
            lst.append(tsim_algs[i])

        theta_resp = tt.stacklists(lst)

    """
        pm.Multinomial('resp', n=ns, p = theta_resp, 
               shape=(TOTAL_PARTS, N_RESPS), observed=data)


        #step = pm.Metropolis()
        trace = pm.sample(MCMC_STEPS,njobs=MCMC_CHAINS,
            tune=BURNIN,target_accept=0.5, thin=MCMC_THIN)
        print_star("Model Finished!")



    summary = pm.df_summary(trace)

    #for t in trace:
       # print t['noise']
    fig, axs = plt.subplots(4, 2) # 3 RVs
    sv = pm.traceplot(trace, ax=axs)
    fig.savefig("trace.png")

    return trace, summary






if __name__ == "__main__":

    #make parentheses lists, 
    #and hypotheses (e.g. OOMC)

    MCMC_STEPS = 250
    MCMC_THIN = 10
    MCMC_CHAINS=1
    BURNIN = 50


    paren_lst = make_lists()
    hyps = make_lists(prims=["(", "[", "]", ")"])
    #hyps = make_lists(prims=["(","[", "]", ")", "O", "C", "M"])
    hyps.append("OOMM")
    hyps.append("OOCC")
    hyps.append("OO)]")
    hyps.append("OO])")
    hyps.append("OMOM")
    hyps.append("([CC")
    hyps.append("[(CC")

    gen = get_hyps_gen(hyps)

    filt = filter_hyps(copy.deepcopy(gen),
                         thresh=0.5, rem_dup=True)


    alg_names = [x for x in filt]
    alg_types = get_algs_of_type(filt)


    ##############################################

    ##extract data from files
    careAbout = "Order pressed"
    monkey_data = getCountData("stevesdata/RecursionMonkey.csv", 

                                careAbout, "Monkeys",
                                subset={"Exposure": "2"})

    kids_data = getCountData("stevesdata/RecursionKids.csv", 
                                careAbout, "Kids")

    tsimane_data = getCountData("stevesdata/RecursionTsimane.csv", 
                                careAbout, "Tsimane")

    #################################################

    data_assignments = lst_format_data(paren_lst,
                 monkey_data,
                     kids_data, 
                     tsimane_data)

    data = data_assignments[0]
    assignments = data_assignments[1]
    groups = ["monkeys", "kids","tsimane"]
    alg_0 = get_0_columns(format_algs(paren_lst,filt, sm=0.0))
    dat_0 = get_0_columns(data)
    both_0 = list(alg_0.intersection(dat_0))

    paren_lst = np.delete(np.array(paren_lst), both_0)
    algorithms = format_algs(paren_lst,filt, sm=0.05)

    x = 0
    data = np.delete(data, both_0, axis=1)
    #algorithms = np.delete(algorithms, both_0, axis=1)
    #algorithms =  algorithms/algorithms.sum(axis=1)[:,None]



    N_GROUPS = len(groups)
    TOTAL_SAMPLES = np.sum(data)
    TOTAL_PARTS = len(data)
    N_ALGS = len(algorithms)
    N_RESPS = len(algorithms[0])
    #N_ALGS = len()


    print_star("TOTAL_SAMPLES", TOTAL_SAMPLES)
    print_star("N_ALGS",N_ALGS)
    print_star("TOTAL_PARTS", TOTAL_PARTS)
    print_star("N_RESPS", N_RESPS)




    trace, model_out = model()



    means= model_out['mean']
    sds = model_out['sd']

    ###################################################

    output_full_alpha_noise(trace, 'noise',  
        names=groups, thin=MCMC_THIN, out="model_out/noise_full.csv")
    output_full_alpha_noise(trace, 'alpha',  
        names=groups, thin=MCMC_THIN, out="model_out/alpha_full.csv")
    output_full_theta_beta(trace, 'beta',  groups=groups,
        names=alg_names, thin=MCMC_THIN, out="model_out/beta_full.csv")

    output_full_theta_beta(trace, 'theta', groups=np.array(groups)[assignments],
        names=alg_names, thin=MCMC_THIN, out="model_out/theta_full.csv")
    grouped = group_vars(means, ["alpha", "beta", "theta", "noise"])
    alpha = grouped["alpha"]
    beta = grouped["beta"]
    theta = grouped["theta"]
    noise = grouped["noise"]

    grouped_sds = group_vars(sds, ["alpha", "beta", "theta", "noise"])
    alpha_sd = grouped_sds["alpha"]
    beta_sd = grouped_sds["beta"]
    theta_sd = grouped_sds["theta"]
    noise_sd = grouped_sds["noise"]

    noise_names = noise[0]
    noise_vals = noise[1]
    noise_sds = noise_sd[1]
    alpha_names = alpha[0]
    alpha_vals = alpha[1]
    alpha_sds = alpha_sd[1]
    beta_names = np.array(beta[0]).reshape(N_GROUPS, N_ALGS)
    beta_vals = np.array(beta[1]).reshape(N_GROUPS, N_ALGS)
    beta_sds = np.array(beta_sd[1]).reshape(N_GROUPS, N_ALGS)
    theta_names = np.array(theta[0]).reshape(TOTAL_PARTS, N_ALGS)
    theta_vals = np.array(theta[1]).reshape(TOTAL_PARTS, N_ALGS)
    theta_sds = np.array(theta_sd[1]).reshape(TOTAL_PARTS, N_ALGS)
        
    output_alphas(groups, alpha_vals, alpha_sds, "model_out/alphas.csv")
    output_alphas(groups, noise_vals, noise_sds, "model_out/noise.csv")   
  
    output_betas(beta_names, beta_vals, beta_sds, groups, 
                    alg_names, alg_types,"model_out/betas.csv") 

    group_names = [groups[a] for a in assignments]
    output_thetas(beta_names, theta_vals, theta_sds, group_names, 
                    alg_names, alg_types,"model_out/thetas.csv")   

    recursive_betas = amount_alg_type(alg_types, beta_vals, 
                                    which_type="Recursive")
    crossing_betas = amount_alg_type(alg_types, beta_vals, 
                                    which_type="Crossing")
    tail_betas = amount_alg_type(alg_types, beta_vals, 
                                    which_type="Tail")

    print_star("Noise", groups, noise_vals)
    print_star("Recursive Betas", recursive_betas)
    print_star("Crossing Betas", crossing_betas)
    print_star("Tail Betas", tail_betas)



    #################################################
    print_star("Alpha", groups, alpha_vals)
    ind = np.arange(len(alpha_names))
    #width = 0.35

    fig, ax = plt.subplots()
    ax.bar(ind, alpha_vals)
    #ax.set_xticks(np.arange(len(alpha_names)))
    ax.set_xticks(ind)
    ax.set_xticklabels(groups)
    fig.savefig("alpha.png")

    ####################################################

    ind = np.arange(len(beta_names[0]))
    fig, ax = plt.subplots()
    ax.bar(ind, beta_vals[0])
    #ax.set_xticks(np.arange(len(alpha_names)))
    ax.set_xticks(ind)
    ax.set_xticklabels(alg_names)
    fig.savefig("beta.png")

    #####################################################


    #g = sns.factorplot(x=)
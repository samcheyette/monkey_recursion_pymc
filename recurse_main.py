
import pymc3 as pm
import theano as T
from helpers import *
from clean_steves_data import *
import matplotlib.pyplot as plt
import seaborn as sns





def model(n_steps=5,burnin=5):
    global data
    data = data.flatten()

    with pm.Model() as m:
        alpha = pm.Exponential('alpha', 1.0, shape=(N_GROUPS,1))
        beta = pm.Dirichlet('beta', np.ones(N_ALGS), 
                            shape=(N_GROUPS,N_ALGS))

        theta = pm.Dirichlet('theta', alpha[assignments] * beta[assignments], 
                            shape=(TOTAL_PARTS,N_ALGS))


       # s = Categorical('s', algorithms, shape=(5,8))
        theta_resp = theta.dot(algorithms)
        #samp1 = random.randint(0, N_GROUPS*)

        theta_resp = (theta_resp.flatten() * 
                1/float(TOTAL_PARTS))

        algs = pm.Multinomial('algs', TOTAL_SAMPLES,
                            theta_resp,
                        shape=(TOTAL_SAMPLES),
                        observed=data)

        trace = pm.sample(n_steps, tune=burnin)
        print_star("Model Finished!")

        #map_ = pm.find_MAP()



    summary = pm.df_summary(trace)

    fig, axs = plt.subplots(3, 2) # 3 RVs
    sv = pm.traceplot(trace, ax=axs)
    fig.savefig("trace.png")

    return summary





if __name__ == "__main__":

    #make parentheses lists, 
    #and hypotheses (e.g. OOMC)

    MCMC_STEPS = 3000
    BURNIN = 200

    paren_lst = make_lists()
    hyps = make_lists(prims=["(", "[", "]", ")",'O','C','M'])
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
    algorithms = format_algs(paren_lst,filt)
    groups = ["monkeys", "kids","tsimane"]

    N_GROUPS = len(groups)
    TOTAL_SAMPLES = np.sum(data)
    TOTAL_PARTS = len(data)
    N_ALGS = len(algorithms)
    #N_ALGS = len()


    print_star("TOTAL_SAMPLES", TOTAL_SAMPLES)
    print_star("N_ALGS",N_ALGS)
    print_star("TOTAL_PARTS", TOTAL_PARTS)

    model_out = model(MCMC_STEPS, BURNIN)



    means= model_out['mean']
    sds = model_out['sd']

    ###################################################


    grouped = group_vars(means, ["alpha", "beta", "theta"])
    alpha = grouped["alpha"]
    beta = grouped["beta"]
    theta = grouped["theta"]

    grouped_sds = group_vars(sds, ["alpha", "beta", "theta"])
    alpha_sd = grouped_sds["alpha"]
    beta_sd = grouped_sds["beta"]
    theta_sd = grouped_sds["theta"]

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
    print_star("Recursive Betas", recursive_betas)
    print_star("Crossing Betas", crossing_betas)
    print_star("Tail Betas", tail_betas)



    #################################################
    print_star("Alpha", alpha_names, alpha_vals)
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
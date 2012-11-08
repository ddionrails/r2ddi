eapply                  package:base                   R Documentation

_A_p_p_l_y _a _F_u_n_c_t_i_o_n _O_v_e_r _V_a_l_u_e_s _i_n _a_n _E_n_v_i_r_o_n_m_e_n_t

_D_e_s_c_r_i_p_t_i_o_n:

     ‘eapply’ applies ‘FUN’ to the named values from an ‘environment’
     and returns the results as a list.  The user can request that all
     named objects are used (normally names that begin with a dot are
     not).  The output is not sorted and no enclosing environments are
     searched.

     This is a primitive function.

_U_s_a_g_e:

     eapply(env, FUN, ..., all.names = FALSE, USE.NAMES = TRUE)
     
_A_r_g_u_m_e_n_t_s:

     env: environment to be used.

     FUN: the function to be applied, found _via_ ‘match.fun’.  In the
          case of functions like ‘+’, ‘%*%’, etc., the function name
          must be backquoted or quoted.

     ...: optional arguments to ‘FUN’.

all.names: a logical indicating whether to apply the function to all
          values.

USE.NAMES: logical indicating whether the resulting list should have
          ‘names’.

_V_a_l_u_e:

     A named (unless ‘USE.NAMES = FALSE’) list.  Note that the order of
     the components is arbitrary for hashed environments.

_S_e_e _A_l_s_o:

     ‘environment’, ‘lapply’.

_E_x_a_m_p_l_e_s:

     require(stats)
     
     env <- new.env(hash = FALSE) # so the order is fixed
     env$a <- 1:10
     env$beta <- exp(-3:3)
     env$logic <- c(TRUE, FALSE, FALSE, TRUE)
     # what have we there?
     utils::ls.str(env)
     
     # compute the mean for each list element
            eapply(env, mean)
     unlist(eapply(env, mean, USE.NAMES = FALSE))
     
     # median and quartiles for each element (making use of "..." passing):
     eapply(env, quantile, probs = 1:3/4)
     eapply(env, quantile)
     


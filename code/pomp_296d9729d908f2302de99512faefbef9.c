/* pomp C snippet file: pomp_296d9729d908f2302de99512faefbef9 */
/* Time: 2020-04-14 14:55:57.231 -0400 */
/* Salt: 4544AA5BF480E7409DC7AB37 */

#include <C:/Users/andre/Documents/R/win-library/3.6/pomp/include/pomp.h>
#include <R_ext/Rdynload.h>

 


/* C snippet: 'dprior' */
#define log_beta_s		(__p[__parindex[0]])
#define trans_e		(__p[__parindex[1]])
#define trans_a		(__p[__parindex[2]])
#define trans_c		(__p[__parindex[3]])
#define trans_h		(__p[__parindex[4]])
#define log_g_e		(__p[__parindex[5]])
#define log_g_a		(__p[__parindex[6]])
#define log_g_su		(__p[__parindex[7]])
#define log_g_sd		(__p[__parindex[8]])
#define log_g_c		(__p[__parindex[9]])
#define log_g_h		(__p[__parindex[10]])
#define log_max_diag		(__p[__parindex[11]])
#define log_diag_inc_rate		(__p[__parindex[12]])
#define max_detect_par		(__p[__parindex[13]])
#define log_detect_inc_rate		(__p[__parindex[14]])
#define frac_asym		(__p[__parindex[15]])
#define frac_hosp		(__p[__parindex[16]])
#define frac_dead		(__p[__parindex[17]])
#define log_theta_cases		(__p[__parindex[18]])
#define log_theta_hosps		(__p[__parindex[19]])
#define log_theta_deaths		(__p[__parindex[20]])
#define E1_0		(__p[__parindex[21]])
#define E2_0		(__p[__parindex[22]])
#define E3_0		(__p[__parindex[23]])
#define E4_0		(__p[__parindex[24]])
#define Ia1_0		(__p[__parindex[25]])
#define Ia2_0		(__p[__parindex[26]])
#define Ia3_0		(__p[__parindex[27]])
#define Ia4_0		(__p[__parindex[28]])
#define Isu1_0		(__p[__parindex[29]])
#define Isu2_0		(__p[__parindex[30]])
#define Isu3_0		(__p[__parindex[31]])
#define Isu4_0		(__p[__parindex[32]])
#define Isd1_0		(__p[__parindex[33]])
#define Isd2_0		(__p[__parindex[34]])
#define Isd3_0		(__p[__parindex[35]])
#define Isd4_0		(__p[__parindex[36]])
#define lik		(__lik[0])

void __pomp_dprior (double *__lik, const double *__p, int give_log, const int *__parindex)
{
 lik = dnorm(log_beta_s, -17.0927194398423,8, 1) + dnorm(trans_e, 2,8, 1) + dnorm(trans_a, 2,8, 1) + dnorm(trans_c, 1,8, 1) + dnorm(trans_h, 10,8, 1) + dnorm(log_g_e, 0,8, 1) + dnorm(log_g_a, 0.133531392624523,8, 1) + dnorm(log_g_su, -0.405465108108164,8, 1) + dnorm(log_g_sd, 0.287682072451781,8, 1) + dnorm(log_g_c, 0.287682072451781,8, 1) + dnorm(log_g_h, -1.09861228866811,8, 1) + dnorm(log_max_diag, 0,8, 1) + dnorm(log_diag_inc_rate, -3,8, 1) + dnorm(max_detect_par, 0,8, 1) + dnorm(log_detect_inc_rate, -3,8, 1) + dnorm(frac_asym, 1.5,8, 1) + dnorm(frac_hosp, 3,8, 1) + dnorm(frac_dead, 1.2,8, 1) + dnorm(log_theta_cases, 2.30258509299405,8, 1) + dnorm(log_theta_hosps, 2.30258509299405,8, 1) + dnorm(log_theta_deaths, 2.30258509299405,8, 1) + dnorm(E1_0, 40, 5, 1) + dnorm(E2_0, 40, 5, 1) + dnorm(E3_0, 40, 5, 1) + dnorm(E4_0, 40, 5, 1) + dnorm(Ia1_0, 22, 4, 1) + dnorm(Ia2_0, 22, 4, 1) + dnorm(Ia3_0, 22, 4, 1) + dnorm(Ia4_0, 22, 4, 1) + dnorm(Isu1_0, 90, 7, 1) + dnorm(Isu2_0, 90, 7, 1) + dnorm(Isu3_0, 90, 7, 1) + dnorm(Isu4_0, 90, 7, 1) + dnorm(Isd1_0, 14, 3, 1) + dnorm(Isd2_0, 14, 3, 1) + dnorm(Isd3_0, 14, 3, 1) + dnorm(Isd4_0, 14, 3, 1) ; 
 if (!give_log) lik = exp(lik); 
}

#undef log_beta_s
#undef trans_e
#undef trans_a
#undef trans_c
#undef trans_h
#undef log_g_e
#undef log_g_a
#undef log_g_su
#undef log_g_sd
#undef log_g_c
#undef log_g_h
#undef log_max_diag
#undef log_diag_inc_rate
#undef max_detect_par
#undef log_detect_inc_rate
#undef frac_asym
#undef frac_hosp
#undef frac_dead
#undef log_theta_cases
#undef log_theta_hosps
#undef log_theta_deaths
#undef E1_0
#undef E2_0
#undef E3_0
#undef E4_0
#undef Ia1_0
#undef Ia2_0
#undef Ia3_0
#undef Ia4_0
#undef Isu1_0
#undef Isu2_0
#undef Isu3_0
#undef Isu4_0
#undef Isd1_0
#undef Isd2_0
#undef Isd3_0
#undef Isd4_0
#undef lik

static int __pomp_load_stack = 0;

void __pomp_load_stack_incr (void) {++__pomp_load_stack;}

void __pomp_load_stack_decr (int *val) {*val = --__pomp_load_stack;}

void R_init_pomp_296d9729d908f2302de99512faefbef9 (DllInfo *info)
{
R_RegisterCCallable("pomp_296d9729d908f2302de99512faefbef9", "__pomp_load_stack_incr", (DL_FUNC) __pomp_load_stack_incr);
R_RegisterCCallable("pomp_296d9729d908f2302de99512faefbef9", "__pomp_load_stack_decr", (DL_FUNC) __pomp_load_stack_decr);
R_RegisterCCallable("pomp_296d9729d908f2302de99512faefbef9", "__pomp_dprior", (DL_FUNC) __pomp_dprior);
}

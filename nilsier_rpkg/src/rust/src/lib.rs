use nilsier::Nils;
use savvy::{
    // IntegerSexp,
    OwnedIntegerSexp,
    // RealSexp,
    Sexp,
    savvy,
    savvy_err,
};

#[savvy]
fn nils_estimate(psus: a) -> savvy::Result<Sexp> {
    let mut nils = Nils::new(psus, nn_sizes, capacity);
    let mut out = OwnedIntegerSexp::new(1);
    out[0] = 1;
    Ok(Sexp::from(out))
}

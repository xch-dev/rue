use std::collections::HashMap;

use log::debug;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BinaryOp, Builtin, Hir, HirId, UnaryOp, Value};
use rue_lir::ClvmOp;
use rue_parser::SyntaxToken;
use rue_types::{Pair, Type, TypeId, Union, substitute_with_mappings};

use crate::{Compiler, compile_expr};

/// Builtins don't have a function type, since some of them accept more than one shape of
/// arguments, so each one checks its own arguments here.
pub(crate) fn compile_builtin(
    ctx: &mut Compiler,
    call: &AstFunctionCallExpr,
    builtin: Builtin,
) -> Value {
    let call = BuiltinCall::new(ctx, call);
    let types = ctx.builtins().types;

    match builtin {
        Builtin::Sha256 { inline } => call.hash(
            ctx,
            inline,
            [UnaryOp::Sha256, UnaryOp::Sha256Inline],
            ClvmOp::Sha256,
        ),
        Builtin::Keccak256 { inline } => call.hash(
            ctx,
            inline,
            [UnaryOp::Keccak256, UnaryOp::Keccak256Inline],
            ClvmOp::Keccak256,
        ),
        Builtin::Concat => call.list_op(ctx, ClvmOp::Concat, types.bytes, types.bytes),
        Builtin::Sum => call.list_op(ctx, ClvmOp::Add, types.int, types.int),
        Builtin::Difference => call.list_op(ctx, ClvmOp::Sub, types.int, types.int),
        Builtin::Product => call.list_op(ctx, ClvmOp::Mul, types.int, types.int),
        Builtin::Any => call.list_op(ctx, ClvmOp::Any, types.bool, types.bool),
        Builtin::All => call.list_op(ctx, ClvmOp::All, types.bool, types.bool),
        Builtin::G1Sum => call.list_op(ctx, ClvmOp::G1Add, types.public_key, types.public_key),
        Builtin::G1Difference => {
            call.list_op(ctx, ClvmOp::G1Subtract, types.public_key, types.public_key)
        }
        Builtin::G2Sum => call.list_op(ctx, ClvmOp::G2Add, types.signature, types.signature),
        Builtin::G2Difference => {
            call.list_op(ctx, ClvmOp::G2Subtract, types.signature, types.signature)
        }
        Builtin::CoinId => {
            let hir = call.fixed(ctx, [types.bytes, types.bytes, types.int]).map(
                |[parent, puzzle_hash, amount]| {
                    ctx.alloc_hir(Hir::CoinId(parent, puzzle_hash, amount))
                },
            );
            value_or_unresolved(ctx, hir, types.bytes32)
        }
        Builtin::Substr => {
            call.expect_between(ctx, 2, 3);
            call.reject_spread(ctx);

            let hir = (call.args.len() >= 2).then(|| {
                let value = call.check(ctx, 0, types.bytes);
                let start = call.check(ctx, 1, types.int);
                let end = (call.args.len() == 3).then(|| call.check(ctx, 2, types.int));
                ctx.alloc_hir(Hir::Substr(value, start, end))
            });

            value_or_unresolved(ctx, hir, types.bytes)
        }
        Builtin::Divmod => {
            let hir = call
                .fixed(ctx, [types.int, types.int])
                .map(|[left, right]| ctx.alloc_hir(Hir::Binary(BinaryOp::Divmod, left, right)));
            let pair = ctx.alloc_type(Type::Pair(Pair::new(types.int, types.int)));
            value_or_unresolved(ctx, hir, pair)
        }
        Builtin::Modpow => {
            let hir = call.fixed(ctx, [types.int, types.int, types.int]).map(
                |[base, exponent, modulus]| ctx.alloc_hir(Hir::Modpow(base, exponent, modulus)),
            );
            value_or_unresolved(ctx, hir, types.int)
        }
        Builtin::PubkeyForExp => {
            // The exponent is interpreted as a signed integer and reduced modulo the group
            // order, so both integers and 32 byte secret keys are accepted.
            let exponent = ctx.alloc_type(Type::Union(Union::new(vec![types.int, types.bytes32])));
            let hir = call
                .fixed(ctx, [exponent])
                .map(|[exponent]| ctx.alloc_hir(Hir::Unary(UnaryOp::PubkeyForExp, exponent)));
            value_or_unresolved(ctx, hir, types.public_key)
        }
        Builtin::G1Map => call.map_to_curve(ctx, Hir::G1Map, types.public_key),
        Builtin::G2Map => call.map_to_curve(ctx, Hir::G2Map, types.signature),
        Builtin::BlsPairingIdentity => {
            let hir = if call.spread.is_some() {
                call.expect_count(ctx, 1);
                let pairs = alternating_list_of(ctx, types.public_key, types.signature);
                call.check_arg(ctx, 0, pairs)
                    .map(|pairs| ctx.alloc_hir(Hir::ClvmOp(ClvmOp::BlsPairingIdentity, pairs)))
            } else {
                if !call.args.len().is_multiple_of(2) {
                    ctx.diagnostic(call.call.syntax(), DiagnosticKind::ExpectedEvenArguments);
                }

                let args = (0..call.args.len())
                    .map(|i| {
                        let ty = if i % 2 == 0 {
                            types.public_key
                        } else {
                            types.signature
                        };
                        call.check(ctx, i, ty)
                    })
                    .collect();

                Some(ctx.alloc_hir(Hir::BlsPairingIdentity(args)))
            };

            value_or_unresolved(ctx, hir, types.nil)
        }
        Builtin::BlsVerify => {
            let hir = if call.spread.is_some() {
                call.expect_count(ctx, 2);
                let pairs = alternating_list_of(ctx, types.public_key, types.bytes);

                (call.args.len() == 2).then(|| {
                    let signature = call.check(ctx, 0, types.signature);
                    let pairs = call.check(ctx, 1, pairs);
                    let args = ctx.alloc_hir(Hir::Pair(signature, pairs));
                    ctx.alloc_hir(Hir::ClvmOp(ClvmOp::BlsVerify, args))
                })
            } else {
                if call.args.len() % 2 != 1 {
                    ctx.diagnostic(
                        call.call.syntax(),
                        DiagnosticKind::ExpectedOneArgumentEvenAdditional,
                    );
                }

                let args: Vec<HirId> = (0..call.args.len())
                    .map(|i| {
                        let ty = if i == 0 {
                            types.signature
                        } else if i % 2 == 1 {
                            types.public_key
                        } else {
                            types.bytes
                        };
                        call.check(ctx, i, ty)
                    })
                    .collect();

                args.split_first().map(|(signature, pairs)| {
                    ctx.alloc_hir(Hir::BlsVerify(*signature, pairs.to_vec()))
                })
            };

            value_or_unresolved(ctx, hir, types.nil)
        }
        Builtin::Secp256K1Verify => {
            let hir = call
                .fixed(
                    ctx,
                    [types.k1_public_key, types.bytes32, types.k1_signature],
                )
                .map(|[public_key, message, signature]| {
                    ctx.alloc_hir(Hir::Secp256K1Verify(public_key, message, signature))
                });
            value_or_unresolved(ctx, hir, types.nil)
        }
        Builtin::Secp256R1Verify => {
            let hir = call
                .fixed(
                    ctx,
                    [types.r1_public_key, types.bytes32, types.r1_signature],
                )
                .map(|[public_key, message, signature]| {
                    ctx.alloc_hir(Hir::Secp256R1Verify(public_key, message, signature))
                });
            value_or_unresolved(ctx, hir, types.nil)
        }
    }
}

/// The compiled arguments of a builtin call, before they're checked against the builtin's
/// parameters.
struct BuiltinCall<'a> {
    call: &'a AstFunctionCallExpr,
    args: Vec<(Value, AstExpr)>,
    /// The spread operator on the last argument, if there is one.
    spread: Option<SyntaxToken>,
}

impl<'a> BuiltinCall<'a> {
    fn new(ctx: &mut Compiler, call: &'a AstFunctionCallExpr) -> Self {
        let mut args = Vec::new();
        let mut spread = None;

        let len = call.args().count();

        for (i, arg) in call.args().enumerate() {
            let Some(expr) = arg.expr() else {
                debug!("Unresolved clvm op argument");
                continue;
            };

            if let Some(op) = arg.spread() {
                if i == len - 1 {
                    spread = Some(op);
                } else {
                    ctx.diagnostic(arg.syntax(), DiagnosticKind::NonFinalSpread);
                }
            }

            let value = compile_expr(ctx, &expr, None);

            args.push((value, expr));
        }

        Self { call, args, spread }
    }

    fn expect_count(&self, ctx: &mut Compiler, count: usize) {
        if self.args.len() != count {
            ctx.diagnostic(
                self.call.syntax(),
                DiagnosticKind::ExpectedArguments(count, self.args.len()),
            );
        }
    }

    fn expect_between(&self, ctx: &mut Compiler, min: usize, max: usize) {
        if !(min..=max).contains(&self.args.len()) {
            ctx.diagnostic(
                self.call.syntax(),
                DiagnosticKind::ExpectedArgumentsBetween(min, max, self.args.len()),
            );
        }
    }

    fn reject_spread(&self, ctx: &mut Compiler) {
        if let Some(spread) = &self.spread {
            ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
        }
    }

    /// Checks the argument at `index`, which must exist, against `ty`.
    fn check(&self, ctx: &mut Compiler, index: usize, ty: TypeId) -> HirId {
        let (value, expr) = &self.args[index];
        ctx.assign_type(expr.syntax(), value.ty, ty);
        value.hir
    }

    /// Checks the argument at `index` against `ty`, if it was passed.
    fn check_arg(&self, ctx: &mut Compiler, index: usize, ty: TypeId) -> Option<HirId> {
        (index < self.args.len()).then(|| self.check(ctx, index, ty))
    }

    /// Checks a call with exactly one argument per parameter and no spread. The arguments are
    /// only checked if there are the right number of them.
    fn fixed<const N: usize>(&self, ctx: &mut Compiler, params: [TypeId; N]) -> Option<[HirId; N]> {
        self.expect_count(ctx, N);
        self.reject_spread(ctx);

        (self.args.len() == N).then(|| std::array::from_fn(|i| self.check(ctx, i, params[i])))
    }

    /// `sha256` and `keccak256` hash one value, or the concatenation of a spread list of values.
    /// Only the spread form needs the CLVM operator, since the value is otherwise already one
    /// atom.
    fn hash(
        &self,
        ctx: &mut Compiler,
        inline: bool,
        [op, inline_op]: [UnaryOp; 2],
        spread_op: ClvmOp,
    ) -> Value {
        let types = ctx.builtins().types;

        self.expect_count(ctx, 1);

        let ty = if let Some(spread) = &self.spread {
            if inline {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }
            list_of(ctx, types.bytes)
        } else {
            types.bytes
        };

        let value = self
            .check_arg(ctx, 0, ty)
            .unwrap_or_else(|| ctx.unresolved().hir);

        let hir = match (self.spread.is_some(), inline) {
            (true, _) => Hir::ClvmOp(spread_op, value),
            (false, true) => Hir::Unary(inline_op, value),
            (false, false) => Hir::Unary(op, value),
        };

        Value::new(ctx.alloc_hir(hir), types.bytes32)
    }

    /// A builtin that applies `op` to a single list of `item` values.
    fn list_op(&self, ctx: &mut Compiler, op: ClvmOp, item: TypeId, ret: TypeId) -> Value {
        self.expect_count(ctx, 1);
        self.reject_spread(ctx);

        let list = list_of(ctx, item);
        let value = self
            .check_arg(ctx, 0, list)
            .unwrap_or_else(|| ctx.unresolved().hir);

        Value::new(ctx.alloc_hir(Hir::ClvmOp(op, value)), ret)
    }

    /// `g1_map` and `g2_map` hash a value onto a curve, with an optional domain separation tag.
    fn map_to_curve(
        &self,
        ctx: &mut Compiler,
        map: fn(HirId, Option<HirId>) -> Hir,
        ret: TypeId,
    ) -> Value {
        let bytes = ctx.builtins().types.bytes;

        self.expect_between(ctx, 1, 2);
        self.reject_spread(ctx);

        let data = self
            .check_arg(ctx, 0, bytes)
            .unwrap_or_else(|| ctx.unresolved().hir);
        let dst = self.check_arg(ctx, 1, bytes);

        Value::new(ctx.alloc_hir(map(data, dst)), ret)
    }
}

fn value_or_unresolved(ctx: &mut Compiler, hir: Option<HirId>, ty: TypeId) -> Value {
    let hir = hir.unwrap_or_else(|| ctx.unresolved().hir);
    Value::new(hir, ty)
}

fn list_of(ctx: &mut Compiler, item: TypeId) -> TypeId {
    let types = ctx.builtins().types;
    let mappings = HashMap::from([(types.list_generic, item)]);
    substitute_with_mappings(ctx.types_mut(), types.list, &mappings)
}

fn alternating_list_of(ctx: &mut Compiler, a: TypeId, b: TypeId) -> TypeId {
    let types = ctx.builtins().types;
    let mappings = HashMap::from([
        (types.alternating_list_generic_a, a),
        (types.alternating_list_generic_b, b),
    ]);
    substitute_with_mappings(ctx.types_mut(), types.alternating_list, &mappings)
}

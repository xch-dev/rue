/*
Type::PossiblyUndefined(inner) if field_name.text() == "exists" => {
                let maybe_nil_reference = self.db.alloc_hir(Hir::Op(Op::Exists, old_value.hir_id));
                let exists = self.db.alloc_hir(Hir::Op(Op::Listp, maybe_nil_reference));
                let mut new_value = Value::new(exists, self.builtins.bool);

                if let Some(guard_path) = old_value.guard_path {
                    new_value
                        .guards
                        .insert(guard_path, Guard::new(inner, old_value.type_id));
                }

                new_value
            } */

use baboon_rs_conversion_regression::rust::conversion::{from_1_0_0_holder::convert__holder__from__1_0_0, v1_0_0::holder::Holder};

#[test]
fn optional_to_list_preserves_present_value() {
    let old = Holder { values: Some(42) };
    assert_eq!(convert__holder__from__1_0_0(&old).values, vec![42]);
}

#[test]
fn optional_to_list_preserves_absence() {
    let old = Holder { values: None };
    assert!(convert__holder__from__1_0_0(&old).values.is_empty());
}

#[test]
fn recursive_collections_widen_elements_and_preserve_order() {
    use baboon_rs_conversion_regression::rust::conversion::{from_1_0_0_collections::convert__collections__from__1_0_0, v1_0_0::collections::Collections};
    let old = Collections {
        by_name: [("empty".to_owned(), None), ("present".to_owned(), Some(-123))].into(),
        unique: vec![3, -2, 3],
        wrapped: i32::MAX,
    };
    let new = convert__collections__from__1_0_0(&old);
    assert_eq!(new.by_name["present"], vec![-123i64]);
    assert!(new.by_name["empty"].is_empty());
    assert_eq!(new.unique.into_iter().collect::<Vec<_>>(), vec![-2i64, 3]);
    assert_eq!(new.wrapped, Some(i64::from(i32::MAX)));
}

#[test]
fn nested_user_types_use_their_evolution_conversions() {
    use baboon_rs_conversion_regression::rust::conversion::{from_1_0_0_nested::convert__nested__from__1_0_0, v1_0_0::{child::Child, nested::Nested}};
    let old = Nested { child: Child { values: Some(7) }, children: vec![Child { values: None }] };
    let new = convert__nested__from__1_0_0(&old);
    assert_eq!(new.child.values, vec![7]);
    assert!(new.children[0].values.is_empty());
}

#[test]
fn adt_branches_use_their_evolution_conversions() {
    use baboon_rs_conversion_regression::rust::conversion::{
        from_1_0_0_event::convert__event__from__1_0_0,
        v1_0_0::event::{Event, Payload},
    };
    let old = Event::Payload(Payload { values: Some(42) });
    let baboon_rs_conversion_regression::rust::conversion::event::Event::Payload(new) = convert__event__from__1_0_0(&old);
    assert_eq!(new.values, vec![42]);
}

#[test]
fn multiple_any_bearing_adt_branches_share_runtime_framing() {
    use baboon_rs_conversion_regression::{
        any_opaque::{AnyMeta, AnyOpaque, AnyOpaqueUeba},
        baboon_runtime::{BaboonBinDecode, BaboonBinEncode, BaboonCodecContext},
        rust::anyadt::envelope::{Envelope, First, Second},
    };
    let any = AnyOpaque::Ueba(AnyOpaqueUeba::new(
        AnyMeta::new(7, Some("domain".to_owned()), Some("1.0.0".to_owned()), Some("type".to_owned())).unwrap(),
        vec![1, 2, 3],
    ));
    for value in [Envelope::First(First { value: any.clone() }), Envelope::Second(Second { values: vec![any] })] {
        for context in [BaboonCodecContext::Compact, BaboonCodecContext::Indexed] {
            let mut wire = Vec::new();
            value.encode_ueba(&context, &mut wire).unwrap();
            assert_eq!(Envelope::decode_ueba(&context, &mut std::io::Cursor::new(wire)).unwrap(), value);
        }
    }
}

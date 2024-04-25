use super::*;

#[component]
pub fn GettingStarted() -> impl IntoView {
    view! {
        <PageTitle id="getting-started">"Getting Started"</PageTitle>
        <Body>
            "Rue is a pure functional programming language that compiles to "
            <Link href="https://chialisp.com/clvm".into()>"CLVM"</Link>
            ". It's strictly typed to prevent common mistakes."
        </Body>
    }
}

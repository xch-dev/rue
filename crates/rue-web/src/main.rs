use leptos::*;
use leptos_router::*;
use thaw::*;

fn main() {
    mount_to_body(App)
}

#[component]
pub fn App() -> impl IntoView {
    let theme = create_rw_signal(Theme::dark());

    view! {
        <Router>
            <ThemeProvider theme>
                <GlobalStyle/>
                <Layout>
                    <Header/>
                    <Layout style="padding: 24px;">
                        <AppRoutes/>
                    </Layout>
                </Layout>
            </ThemeProvider>
        </Router>
    }
}

#[component]
fn AppRoutes() -> impl IntoView {
    view! {
        <Routes>
            <Route path="/" view=Home/>
            <Route path="/docs" view=Docs/>
        </Routes>
    }
}

#[component]
fn Header() -> impl IntoView {
    view! {
        <LayoutHeader style="padding: 24px;">
            <Space align=SpaceAlign::Center justify=SpaceJustify::SpaceBetween>
                <a href="/" class="text-inherit">
                    <Space align=SpaceAlign::Center>
                        <img src="/logo.png" alt="Rue Lang" style="height: 34px;"/>
                        <span style="font-size: 30px;" translate="no" class="notranslate">
                            "Rue Lang"
                        </span>
                    </Space>
                </a>
                <Space gap=SpaceGap::Size(30)>
                    <a href="docs" class="text-inherit">
                        <Icon icon=icondata::FaBookSolid width="32px" height="32px"/>
                    </a>

                    <a href="https://github.com/rigidity/rue" target="_blank" class="text-inherit">
                        <Icon icon=icondata::ImGithub width="32px" height="32px"/>
                    </a>
                </Space>
            </Space>
        </LayoutHeader>
    }
}

#[component]
fn Home() -> impl IntoView {
    view! {
        <div class="mt-16 mx-auto w-11/12 md:w-9/12 lg:w-7/12 xl:w-6/12">
            <h2 class="text-7xl">"Rue"</h2>
            <p class="text-2xl mt-5">
                "A language crafted from the ground up to make developing "
                "smart coin logic on the Chia blockchain easy for everyone."
            </p>
            <a href="/docs">
                <Button class="text-lg mt-5" size=ButtonSize::Large>
                    "Get Started"
                </Button>
            </a>
        </div>
    }
}

#[component]
fn Docs() -> impl IntoView {
    view! {}
}

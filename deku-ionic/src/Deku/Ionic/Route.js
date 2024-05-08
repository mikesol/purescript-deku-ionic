export const beforeEnter = (router) => (cb) => () => {
  router.beforeEnter = cb;
};
export const beforeLeave = (router) => (cb) => () => {
  router.beforeLeave = cb;
};
export const componentProps = (router) => (props) => () => {
  router.componentProps = props;
};
export const getBeforeEnter = (router) => () => router.beforeEnter;
export const getBeforeLeave = (router) => () => router.beforeLeave;
export const getComponentProps = (router) => () => router.componentProps;

//////
export const unsafeCustomComponentImpl =
  (componentName) => (connectedHook) => (disconnectedHook) => (run) => () => {
    class CustomComponent extends HTMLElement {
      constructor() {
        super();
      }
      disconnectedCallback() {
        disconnectedHook();
      }
      connectedCallback() {
        const params = new URLSearchParams(window.location.search);
        const qValue = params.get("q");
        const decodedValue = decodeURIComponent(qValue);
        run(this)(decodedValue)();
        connectedHook();
      }
    }
    window.customElements.define(componentName, CustomComponent);
  };

export const eagerUnsafeCustomComponentImpl =
  (componentName) => (connectedHook) => (disconnectedHook) => (run) => () => {
    const e = document.createElement("div");
    e.setAttribute('style', 'display: none;');
    document.body.appendChild(e);
    run(e)();
    const v = e.firstChild;
    class CustomComponent extends HTMLElement {
      constructor() {
        super();
      }
      disconnectedCallback() {
        disconnectedHook();
      }
      connectedCallback() {
        this.appendChild(v);
        connectedHook();
      }
    }
    window.customElements.define(componentName, CustomComponent);
  };

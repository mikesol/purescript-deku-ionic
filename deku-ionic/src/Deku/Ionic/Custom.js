export const customComponentImpl =
    (componentName) => (connectedHook) => (disconnectedHook) => (run) => () => {
    class CustomComponent extends HTMLElement {
      constructor() {
        super();
      }
      disconnectedCallback() {
        disconnectedHook();
      }
      connectedCallback() {
        run(this)();
        connectedHook();
      }
    }
    window.customElements.define(componentName, CustomComponent);
  };
